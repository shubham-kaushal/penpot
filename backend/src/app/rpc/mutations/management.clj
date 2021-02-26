;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.rpc.mutations.management
  "Move & Duplicate RPC methods for files and projects."
  (:require
   [app.common.exceptions :as ex]
   [app.common.data :as d]
   [app.common.pages.migrations :as pmg]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.db :as db]
   [app.rpc.queries.projects :as proj]
   [app.rpc.queries.teams :as teams]
   [app.util.blob :as blob]
   [app.util.services :as sv]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]))

(s/def ::id ::us/uuid)
(s/def ::profile-id ::us/uuid)
(s/def ::project-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::team-id ::us/uuid)

(defn- remap-id
  [item index key]
  (cond-> item
    (contains? item key)
    (assoc key (get index (get item key) (get item key)))))

(defn- process-file
  [file index]
  (letfn [;; A function responsible to analize all file data and
          ;; replace the old :component-file reference with the new
          ;; ones, using the provided file-index
          (relink-components [data]
            (walk/postwalk (fn [form]
                             (cond-> form
                               (and (map? form) (uuid? (:component-file form)))
                               (update :component-file #(get index % %))))
                           data))

          ;; A function responsible of process the :media attr of file
          ;; data and remap the old ids with the new ones.
          (relink-media [media]
            (reduce-kv (fn [res k v]
                         (let [id (get index k)]
                           (if (uuid? id)
                             (-> res
                                 (assoc id (assoc v :id id))
                                 (dissoc k))
                             res)))
                       media
                       media))]

    (update file :data
            (fn [data]
              (-> data
                  (blob/decode)
                  (pmg/migrate-data)
                  (update :pages-index relink-components)
                  (update :components relink-components)
                  (update :media relink-media)
                  (d/without-nils)
                  (blob/encode))))))

(defn- duplicate-file
  [conn {:keys [profile-id file index project-id]} {:keys [reset-shared-flag] :as opts}]
  (let [flibs  (db/query conn :file-library-rel {:file-id (:id file)})
        fmeds  (db/query conn :file-media-object {:file-id (:id file)})

        ;; Remap all file-librar-rel rows to the new file id
        flibs  (map #(remap-id % index :file-id) flibs)

        ;; Add to the index all non-local file media objects
        index  (reduce #(assoc %1 (:id %2) (uuid/next))
                       index
                       (remove :is-local fmeds))

        ;; Remap all file-media-object rows and assing correct new id
        ;; to each row
        fmeds  (->> fmeds
                    (map #(assoc % :id (or (get index (:id %)) (uuid/next))))
                    (map #(remap-id % index :file-id)))

        file   (cond-> file
                 (some? project-id)
                 (assoc :project-id project-id)

                 (true? reset-shared-flag)
                 (assoc :is-shared false))

        file   (-> file
                   (update :id #(get index %))
                   (process-file index))]

    (db/insert! conn :file file)
    (db/insert! conn :file-profile-rel
                {:file-id (:id file)
                 :profile-id profile-id
                 :is-owner true
                 :is-admin true
                 :can-edit true})

    (doseq [params flibs]
      (db/insert! conn :file-library-rel params))

    (doseq [params fmeds]
      (db/insert! conn :file-media-object params))

    file))


;; --- MUTATION: Duplicate File

(declare duplicate-file)

(s/def ::duplicate-file
  (s/keys :req-un [::profile-id ::file-id]))

(sv/defmethod ::duplicate-file
  [{:keys [pool] :as cfg} {:keys [profile-id file-id] :as params}]
  (db/with-atomic [conn pool]
    (let [file   (db/get-by-id conn :file file-id)
          index  {file-id (uuid/next)}
          params (assoc params :index index :file file)]
      (proj/check-edition-permissions! conn profile-id (:project-id file))
      (-> (duplicate-file conn params {:reset-shared-flag true})
          (update :data blob/decode)))))


;; --- MUTATION: Duplicate Project

(declare duplicate-project)

(s/def ::duplicate-project
  (s/keys :req-un [::profile-id ::project-id]))

(sv/defmethod ::duplicate-project
  [{:keys [pool] :as cfg} {:keys [profile-id project-id] :as params}]
  (db/with-atomic [conn pool]
    (let [project (db/get-by-id conn :project project-id)]
      (teams/check-edition-permissions! conn profile-id (:team-id project))
      (duplicate-project conn (assoc params :project project)))))

(defn duplicate-project
  [conn {:keys [profile-id project] :as params}]
  (let [files   (db/query conn :file
                          {:project-id (:id project)}
                          {:columns [:id]})

        index   (reduce #(assoc %1 (:id %2) (uuid/next)) {} files)
        project (assoc project :id (uuid/next))
        params  (assoc params
                       :project-id (:id project)
                       :index index)]

    (db/insert! conn :project project)
    (db/insert! conn :project-profile-rel {:project-id (:id project)
                                           :profile-id profile-id
                                           :is-owner true
                                           :is-admin true
                                           :can-edit true})
    (doseq [{:keys [id]} files]
      (let [file   (db/get-by-id conn :file id)
            params (assoc params :file file)]
        (duplicate-file conn params {:reset-shared-flag false
                                     :remap-libraries true})))
    project))


;; --- MUTATION: Move file

(declare move-file)

(s/def ::move-file
  (s/keys :req-un [::profile-id ::file-id ::project-id]))

(sv/defmethod ::move-file
  [{:keys [pool] :as cfg} {:keys [profile-id file-id project-id] :as params}]
  (db/with-atomic [conn pool]
    (let [file           (db/get-by-id conn :file file-id {:columns [:id :project-id]})
          src-project-id (:project-id file)
          dst-project-id project-id]

      (proj/check-edition-permissions! conn profile-id src-project-id)
      (proj/check-edition-permissions! conn profile-id dst-project-id)

      (when (= dst-project-id src-project-id)
        (ex/raise :type :validation
                  :code :cant-move-to-same-project
                  :hint "Unable to move a file to the same project"))

      (move-file conn {:profile-id profile-id
                       :file-id file-id
                       :src-project-id src-project-id
                       :dst-project-id dst-project-id})
      nil)))

(def sql:delete-broken-library-relations-for-file
  "with broken as (
     select * from file_library_rel as flr
      inner join file as lf on (flr.library_file_id = lf.id)
      inner join project as lp on (lf.project_id = lp.id)
      where flr.file_id = ?
        and lp.team_id != ?
   )
   delete from file_library_rel as rel
    using broken as br
    where rel.file_id = br.file_id
      and rel.library_file_id = br.library_file_id")

(defn- move-file
  [conn {:keys [profile-id file-id src-project-id dst-project-id] :as params}]
  (let [src-project (db/get-by-id conn :project src-project-id)
        dst-project (db/get-by-id conn :project dst-project-id)
        dst-team-id (:team-id dst-project)]

    ;; Move file to the destination project
    (db/update! conn :file
                {:project-id dst-project-id}
                {:id file-id})

    ;; If the destination project is different from origin; then
    ;; perform a cleanup of broken library relations.
    (when (not= (:team-id src-project)
                (:team-id dst-project))
      (db/exec-one! conn [sql:delete-broken-library-relations-for-file
                          file-id dst-team-id]))))

;; --- MUTATION: Move project

(declare move-project)

(s/def ::move-project
  (s/keys :req-un [::profile-id ::team-id ::project-id]))

(sv/defmethod ::move-project
  [{:keys [pool] :as cfg} {:keys [profile-id team-id project-id] :as params}]
  (db/with-atomic [conn pool]
    (let [project     (db/get-by-id conn :project project-id {:columns [:id :team-id]})
          src-team-id (:team-id project)
          dst-team-id team-id]


      (teams/check-edition-permissions! conn profile-id src-team-id)
      (teams/check-edition-permissions! conn profile-id dst-team-id)

      (when (= src-team-id dst-team-id)
        (ex/raise :type :validation
                  :code :cant-move-to-same-team
                  :hint "Unable to move a project to same team"))

      (move-project conn {:profile-id profile-id
                          :project-id project-id
                          :src-team-id src-team-id
                          :dst-team-id dst-team-id})
      nil)))


(def sql:delete-broken-library-relations-for-project
  "with broken as (
     select * from file_library_rel as flr
      inner join file as f on (flr.file_id = f.id)
      inner join file as lf on (flr.library_file_id = lf.id)
      inner join project as lp on (lf.project_id = lp.id)
      where f.project_id = ?
        and lp.team_id != ?
   )
   delete from file_library_rel as rel
    using broken as br
    where rel.file_id = br.file_id
      and rel.library_file_id = br.library_file_id")

(defn- move-project
  [conn {:keys [profile-id project-id src-team-id dst-team-id] :as params}]
  (db/update! conn :project
              {:team-id dst-team-id}
              {:id project-id})
  (db/exec-one! conn [sql:delete-broken-library-relations-for-project
                      project-id dst-team-id]))
