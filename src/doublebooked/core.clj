(ns doublebooked.core
  (:require [clj-time.format :as tf]
            [clj-time.core :as t]))

;; ---------------------------------------------------
;; ---- CLJ Solution to the double-booked problem ----
;; ---------------------------------------------------
;;
;; This solution accepts a collection of maps, each with a :start and :end key
;; Both :start and :end are string representations of date/times
;; eg. {:start "2018-02-26T14:00:00.000Z" :end "2018-02-26T18:00:00.000Z"}
;;
;; Sample usage: (get-overlapping-events events)

;; -------------------------------------------------
;; ---- String->Date Coersion ----
;; -------------------------------------------------

(defn ->date
  "Converts a string to a clj(s) date
   handling parsing errors"
  [da]
  (try
   (tf/parse  da)
    (catch Exception e
    nil)))

(defn str-before?
  "Accepts two string representations as dates.
   Returns boolean of da 'before' db"
  [da db]
  (let [d1 (->date da)
        d2 (->date db)]
    (t/before? d1 d2)))

;; -------------------------------------------------    
;; ---- Recursive Function ----   
;; -------------------------------------------------

(defn recur-compare
  "Recursively compares a list of events sorted by start time
   Compares the end time of head of the list with the remaining events 
   (ie. events starting after the head)
   Returns a list of sets, with each set containing the pair of 
   overlapping events"
  [{e1e :end :as ev} coll]
  ;; Filter + Transform the overlaps into #{a b}
  (let [res (keep
              (fn [{e2s :start :as ev2}]
                (when (str-before? e2s e1e)
                  #{ev ev2}))
              coll)
        rem  (rest coll)]

    (concat (vec res)
            (when (seq rem)
              ;; recursive step
              (recur-compare (first coll) rem)))))

;; -------------------------------------------------
;; ---- Solution Function ----
;; -------------------------------------------------

(defn get-overlapping-events  
 "Receives a collection of maps with keys:
  :start, :end - both expecting string representations of date/time
  in the form '2018-02-26T12:00:00.000Z'
  Returns a set of sets, with each set containing the pair of
  overlapping events"
 [coll]
 (if (> (count coll) 1)
   ;; Step 1 :: Sort the collection by :start
   (let [s-coll (vec (sort-by :start coll))]
     ;; Step 2 :: recur through the sorted list
     (into #{} (recur-compare (first s-coll) (rest s-coll))))
   #{}))


