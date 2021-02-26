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

(s/def ::move-file
  (s/keys :req-un [::profile-id ::file-id ::project-id]))

(sv/defmethod ::move-file
  [{:keys [pool] :as cfg} {:keys [profile-id file-id project-id] :as params}]
  (db/with-atomic [conn pool]))


;; --- MUTATION: Move project

(s/def ::move-project
  (s/keys :req-un [::profile-id ::team-id ::project-id]))

(sv/defmethod ::move-project
  [{:keys [pool] :as cfg} {:keys [profile-id team-id project-id] :as params}]
  (db/with-atomic [conn pool]))
