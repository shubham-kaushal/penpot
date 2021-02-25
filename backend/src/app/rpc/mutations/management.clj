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
   [app.common.pages :as cp]
   [app.common.pages.migrations :as pmg]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.db :as db]
   [app.rpc.queries.files :as files]
   [app.rpc.queries.projects :as proj]
   [app.tasks :as tasks]
   [app.util.blob :as blob]
   [app.util.services :as sv]
   [app.util.time :as dt]
   [clojure.spec.alpha :as s]))

(s/def ::id ::us/uuid)
(s/def ::profile-id ::us/uuid)
(s/def ::project-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::team-id ::us/uuid)

(defn- remap-ids
  [index keys items]
  (letfn [(remap-key [item key]
            (cond-> item
              (contains? item key)
              (assoc key (get index (get item key) (get item key)))))

          (remap-keys [item]
            (reduce remap-key item keys))]
    (map remap-keys items)))


;; --- MUTATION: Duplicate File

(declare duplicate-file)

(s/def ::duplicate-file
  (s/keys :req-un [::profile-id ::file-id]))

(sv/defmethod ::duplicate-file
  [{:keys [pool] :as cfg} {:keys [profile-id] :as params}]
  (db/with-atomic [conn pool]
    (proj/check-edition-permissions! conn profile-id project-id)
    (duplicate-file conn params)))

(defn duplicate-file
  [conn {:keys [file-id profile-id project-id]}]
  (let [file   (db/get-by-id conn :file file-id)
        flibs  (db/query conn :file-library-rel {:file-id file-id})
        fmeds  (db/query conn :file-media-object {:file-id file-id})

        file   (assoc file
                      :id (uuid/next))
        file   (cond-> file
                 (some? project-id)
                 (assoc :project-id project-id))

        index  {file-id (:id file)}

        flibs  (->> flibs
                    (remap-ids index #{:file-id}))

        fmeds  (->> fmeds
                    (map #(assoc % :id (uuid/next)))
                    (remap-ids index #{:file-id}))

        fprof  {:file-id (:id file)
                :profile-id profile-id
                :is-owner true
                :is-admin true
                :can-edit true}]

    (db/insert! conn :file file)
    (db/insert! conn :file-profile-rel fprof)

    (doseq [params flibs]
      (db/insert! conn :file-library-rel params))

    (doseq [params fmeds]
      (db/insert! conn :file-media-object params))

    file))

;; --- MUTATION: Duplicate File

(declare duplicate-project)

(s/def ::duplicate-project
  (s/keys :req-un [::profile-id ::project-id]))

(sv/defmethod ::duplicate-project
  [{:keys [pool] :as cfg} {:keys [profile-id] :as params}]
  (db/with-atomic [conn pool]
    ;; TODO: permission checks
    (duplicate-project conn params)))

(defn duplicate-project
  [conn {:keys [project-id profile-id] :as params}]
  (let [project (db/get-by-id conn :project project-id)
        files   (db/query conn :file
                          {:project-id project-id}
                          {:columns [:id]})
        project (assoc project :id (uuid/next))
        params  (assoc params :project-id (:id project))]

    (db/insert! conn :project project)
    (db/insert! conn :project-profile-rel {:project-id (:id project)
                                           :profile-id profile-id
                                           :is-owner true
                                           :is-admin true
                                           :can-edit true})
    (doseq [id (map :id files)]
      (duplicate-file conn (assoc params :file-id id)))

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
