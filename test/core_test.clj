(ns core-test
  (:require [clojure.test :refer [deftest is]]
			[doublebooked.core :as db]))

(ns core-test
  (:require [clojure.test :refer [deftest is]]
			[doublebooked.core :as db]))

(def lab {:name "lab"
          :start "2018-02-26T08:00:00.000Z"
          :end "2018-02-26T16:00:00.000Z"})
(def lunch {:name "lunch"
            :start "2018-02-26T08:00:00.000Z"
            :end "2018-02-26T12:00:00.000Z"})
(def basketball {:name "basketball"
                 :start "2018-02-26T10:00:00.000Z"
                 :end "2018-02-26T16:00:00.000Z"})
(def interview {:name "interview"
                :start "2018-02-26T14:01:00.000Z"
                :end "2018-02-26T15:00:00.000Z"})    
(def practice {:name "practice"
               :start "2018-02-26T14:00:00.000Z"
               :end "2018-02-26T18:00:00.000Z"})

(def date1 "2018-02-26T08:00:00.000Z")
(def date2 "2018-02-26T12:00:00.000Z")

(deftest not-before
  (is (= false (db/str-before-or-equal date2 date1))))

(deftest before
  (is (= true (db/str-before-or-equal date1 date2))))

(deftest equal-resolves-as-before
  (is (= true (db/str-before-or-equal date1 date1))))

(deftest one-event-is-empty
  (is (= #{} (db/get-overlapping-events [lunch]))))

(deftest no-overlapping-events
  (is (= #{} (db/get-overlapping-events [lunch interview]))))

(deftest no-overlapping-events2
  (is (= #{} (db/get-overlapping-events [lunch practice]))))

(deftest simple-overlap
  (is (= (db/get-overlapping-events [lab lunch basketball]) 
          #{#{lab lunch} #{lab basketball} #{basketball lunch}})))


(deftest many-overlapa
  (is (= (db/get-overlapping-events [practice interview])
        #{#{interview practice}})))
  

  
(deftest many-overlap
  (is (= (db/get-overlapping-events [lab 
                                     lunch 
                                     basketball 
                                     interview 
                                     practice])
        #{#{lab lunch} 
          #{lab basketball} 
          #{lab interview} 
          #{lab practice} 
          #{lunch basketball}
          #{basketball interview} 
          #{basketball practice}
          #{interview practice}})))
          
          
          