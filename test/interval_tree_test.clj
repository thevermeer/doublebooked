(ns interval-tree-test
  (:require [clojure.test :refer [deftest is]]
			[doublebooked.interval-tree :as db]))

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
                :start "2018-02-26T14:00:00.000Z"
                :end "2018-02-26T15:00:00.000Z"})    
(def practice {:name "practice"
               :start "2018-02-26T14:00:00.000Z"
               :end "2018-02-26T18:00:00.000Z"})

(def date1 "2018-02-26T08:00:00.000Z")
(def date2 "2018-02-26T12:00:00.000Z")


;; The goal here is to demonstrate that if one event
;; ends when another begins, they should not overlap
;; Thus the change in the test from 'before-or-equal'
;; to simply 'before'
(def seq-a {:name "interview"
            :start "2018-02-26T14:00:00.000Z"
            :end "2018-02-26T15:00:00.000Z"})    
(def seq-b {:name "practice"
            :start "2018-02-26T15:00:00.000Z" 
            :end "2018-02-26T18:00:00.000Z"})

(deftest one-event-is-empty
  (is (= #{} (db/get-overlapping-events [lunch]))))

(deftest no-overlapping-events
  (is (= #{} (db/get-overlapping-events [lunch interview]))))

(deftest no-overlapping-events2
  (is (= #{} (db/get-overlapping-events [lunch practice]))))

(deftest simple-overlap
  (is (= (db/get-overlapping-events [lab lunch basketball]) 
          #{#{lab lunch} 
            #{lab basketball} 
            #{basketball lunch}})))

(deftest many-overlapa
  (is (= (db/get-overlapping-events [practice interview])
         #{#{interview practice}})))
        
(deftest should-equal-overlap
  (is (= (db/get-overlapping-events [seq-a seq-b])
         #{})))        
    
(deftest many-overlap
  (is (= (db/get-overlapping-events [lunch 
                                     basketball 
                                     interview 
                                     practice
                                     lab])
        #{#{lab lunch} 
          #{lab basketball} 
          #{lab interview} 
          #{lab practice} 
          #{lunch basketball}
          #{basketball interview} 
          #{basketball practice}
          #{interview practice}})))
          
          
          