(ns doublebooked.interval-tree
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

;; ---------------------------------------------------
;; ---- Leaf Filtering Functions -----
;; ---------------------------------------------------

(defn events-before
  "Accepts a DateTime string and a Collection of Events maps
   Returns a collection of events that end prior to the provided datetime"
  [da coll]
  (filterv
    (fn [{:keys [end]}]
      (neg? (compare end da)))
    coll))

(defn events-after
  "Accepts a DateTime string and a Collection of Events maps
   Returns a collection of events that start after to the provided datetime"
  [da coll]
  (filterv 
    (fn [{:keys [start]}]
      (pos? (compare start da)))
    coll))

(defn events-overlapping
  "Accepts an Event and a collection of Event Maps
   Returns a collection of events the overlap the given reference event"
  [{estart :start eend :end} coll]
  (filterv
    (fn [{:keys [start end]}]
      (or 
        (and (<= (compare start estart) 0)
             (>= (compare end estart) 0))
        (and (<= (compare start eend) 0)
             (>= (compare end eend) 0))))
    coll))

;; ---------------------------------------------------
;; ---- Centered Interval Tree -----
;; ---------------------------------------------------

(defn build-tree
  "Accepts a collection of Event maps
   Recursively enumerates a centered interval tree as per:
   https://en.wikipedia.org/wiki/Interval_tree#Centered_interval_tree
   Returns a nested map, with each leaf containing keys:
   :event, :before, :after, :overlapping"
  [coll]
  (when (seq coll)
    (let [s-coll (vec (sort-by :start coll))
          ev (get s-coll (-> s-coll count (/ 2) (- 0) int))]
     {:event       ev
      :before      (->> s-coll
                        (events-before (:start ev))
                        build-tree)
      :after       (->> s-coll
                        (events-after (:start ev))
                        build-tree)
      :overlapping (events-overlapping ev s-coll)})))

;; ---------------------------------------------------
;; ---- Search Functions -----
;; ---------------------------------------------------

(defn overlapping-events
  "Accepts a nested map of maps representing a centered interval tree
   and an event as a reference
   Returns a collection of sets of overlapping events"
  [tree {start :start :as ev}]
  (concat
      ;; overlapping
      (keep
        (fn [e]
          (when (and (>= (compare start (:start e)) 0)
                     (< (compare start (:end e)) 0)
                     (not= e ev))
            #{e ev}))
        (:overlapping tree))

      ;;before
      (when (and (neg? (compare start (:date tree)))
                 (seq (:before tree)))
        (overlapping-events (:before tree) start))

      ;; after
      (when (and (pos? (compare start (:date tree)))
                 (seq (:after tree)))
        (overlapping-events (:after tree) start))))

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
  (let [tree (build-tree coll)]
    (->>
      coll 
      (mapcat (partial overlapping-events tree))
       ;; set coersion - forces DISTINCT on coll   
      (into #{}))))
