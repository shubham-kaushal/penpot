;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2021 UXBOX Labs SL

(ns app.tests.test-services-management
  (:require
   [app.common.uuid :as uuid]
   [app.db :as db]
   [app.http :as http]
   [app.storage :as sto]
   [app.tests.helpers :as th]
   [clojure.test :as t]
   [buddy.core.bytes :as b]
   [datoteka.core :as fs]))

(t/use-fixtures :once th/state-init)
(t/use-fixtures :each th/database-reset)

(t/deftest duplicate-file
  (let [storage (:app.storage/storage th/*system*)
        sobject (sto/put-object storage {:content (sto/content "content")
                                         :content-type "text/plain"
                                         :other "data"})
        profile (th/create-profile* 1 {:is-active true})
        project (th/create-project* 1 {:team-id (:default-team-id profile)
                                       :profile-id (:id profile)})
        file1   (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:id project)})
        file2   (th/create-file* 2 {:profile-id (:id profile)
                                    :project-id (:id project)
                                    :is-shared true})

        libl    (th/link-file-to-library* {:file-id (:id file1)
                                           :library-id (:id file2)})

        mobj    (th/create-file-media-object* {:file-id (:id file1)
                                               :is-local false
                                               :media-id (:id sobject)})]
    (th/update-file*
     {:file-id (:id file1)
      :profile-id (:id profile)
      :changes [{:type :add-media
                 :object (select-keys mobj [:id :width :height :mtype :name])}]})

    (let [data {::th/type :duplicate-file
                :profile-id (:id profile)
                :file-id (:id file1)}
          out  (th/mutation! data)]

      ;; (th/print-result! out)

      ;; Check tha tresult is correct
      (t/is (nil? (:error out)))
      (let [result (:result out)]

        ;; Check that the returned result is a file but has different
        ;; and different name.
        (t/is (= (:name file1) (:name result)))
        (t/is (not= (:id file1) (:id result)))

        ;; Check that the new file has a correct file library relation
        (let [[item :as rows] (db/query th/*pool* :file-library-rel {:file-id (:id result)})]
          (t/is (= 1 (count rows)))
          (t/is (= (:id file2) (:library-file-id item))))

        ;; Check that the new file has a correct file media objects
        (let [[item :as rows] (db/query th/*pool* :file-media-object {:file-id (:id result)})]
          (t/is (= 1 (count rows)))

          ;; Checj that bot items have different ids
          (t/is (not= (:id item) (:id mobj)))

          ;; check that both file-media-objects points to the same storage object.
          (t/is (= (:media-id item) (:media-id mobj)))
          (t/is (= (:media-id item) (:id sobject)))

          ;; Check if media correctly contains the new file-media-object id
          (t/is (contains? (get-in result [:data :media]) (:id item)))

          ;; And does not contains the old one
          (t/is (not (contains? (get-in result [:data :media]) (:id mobj)))))

        ;; Check the total number of files
        (let [rows (db/query th/*pool* :file {:project-id (:id project)})]
          (t/is (= 3 (count rows))))

        ))))

(t/deftest duplicate-project
  (let [storage (:app.storage/storage th/*system*)
        sobject (sto/put-object storage {:content (sto/content "content")
                                         :content-type "text/plain"
                                         :other "data"})
        profile (th/create-profile* 1 {:is-active true})
        project (th/create-project* 1 {:team-id (:default-team-id profile)
                                       :profile-id (:id profile)})
        file1   (th/create-file* 1 {:profile-id (:id profile)
                                    :project-id (:id project)})
        file2   (th/create-file* 2 {:profile-id (:id profile)
                                    :project-id (:id project)
                                    :is-shared true})

        libl    (th/link-file-to-library* {:file-id (:id file1)
                                           :library-id (:id file2)})
        mobj    (th/create-file-media-object* {:file-id (:id file1)
                                               :is-local false
                                               :media-id (:id sobject)})]

    (th/update-file*
     {:file-id (:id file1)
      :profile-id (:id profile)
      :changes [{:type :add-media
                 :object (select-keys mobj [:id :width :height :mtype :name])}]})


    (let [data {::th/type :duplicate-project
                :profile-id (:id profile)
                :project-id (:id project)}
          out  (th/mutation! data)]

      ;; Check tha tresult is correct
      (t/is (nil? (:error out)))
      (let [result (:result out)]
        ;; Check that they are the same project but different ids
        (t/is (= (:name project) (:name result)))
        (t/is (not= (:id project) (:id result)))

        ;; Check the total number of projects (previously is 2, now is 3)
        (let [rows (db/query th/*pool* :project {:team-id (:default-team-id profile)})]
          (t/is (= 3 (count rows))))

        ;; Check that the new project has the same files
        (let [p1-files (db/query th/*pool* :file
                                 {:project-id (:id project)}
                                 {:order-by [:name]})
              p2-files (db/query th/*pool* :file
                                 {:project-id (:id result)}
                                 {:order-by [:name]})]
          (t/is (= (count p1-files)
                   (count p2-files)))

          ;; check that the both files are equivalent
          (doseq [[fa fb] (map vector p1-files p2-files)]
            (t/is (not= (:id fa) (:id fb)))
            (t/is (= (:name fa) (:name fb)))

            (when (= (:id fa) (:id file1))
              (t/is (false? (b/equals? (:data fa)
                                       (:data fb)))))

            (when (= (:id fa) (:id file2))
              (t/is (true? (b/equals? (:data fa)
                                      (:data fb))))))

          )))))




