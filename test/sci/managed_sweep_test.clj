(ns sci.managed-sweep-test
  "A world releases the values of dead managed state without a fork. A
   long-lived context that creates and drops atoms must not keep their last
   values, and its registry must not grow without a limit."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci])
  (:import [java.lang.ref WeakReference]))

(defn- forkable-context []
  (sci/init {:runtime-mode :forkable}))

(defn- tracked-context
  "A context with `track!`: it keeps a weak reference to its argument in
   `probe` and returns the argument."
  [probe]
  (let [ctx (forkable-context)]
    (sci/intern ctx 'user 'track! (fn [x] (reset! probe (WeakReference. x)) x))
    ctx))

(defn- churn!
  "Create and drop `n` atoms in `ctx`. The registrations start the sweep."
  [ctx n]
  (sci/eval-string* ctx (str "(dotimes [_ " n "] (atom 0)) nil")))

(defn- released?
  "True when GC clears the referent of `probe` while `ctx` creates atoms. Each
   round collects the dead owners, then churns so a sweep runs, then collects
   the released values."
  [probe ctx]
  (loop [round 0]
    (System/gc)
    (churn! ctx 5000)
    (System/gc)
    (cond
      (nil? (.get ^WeakReference @probe)) true
      (< round 20) (recur (inc round))
      :else false)))

(defn- registry [ctx]
  (.-registry ^sci.impl.world.DenseWorld (:sci.impl/world ctx)))

(deftest a-dead-atom-releases-its-value-in-a-world-that-never-forks
  (let [probe (atom nil)
        ctx (tracked-context probe)]
    (sci/eval-string* ctx "(atom (track! (object-array 1024))) nil")
    (is (some? (.get ^WeakReference @probe)))
    (let [released (released? probe ctx)]
      (is released "the cell of the dead atom does not keep its value"))))

(deftest an-atom-that-dies-after-a-fork-releases-its-value-in-the-child
  (let [probe (atom nil)
        parent (tracked-context probe)]
    (sci/eval-string* parent "(def a (atom (track! (object-array 1024))))")
    (let [child (sci/fork parent)]
      (is (= 1024 (sci/eval-string* child "(count @a)")))
      ;; The atom dies in both worlds. The child keeps a copy of its value
      ;; from the fork, and the sweep of the lineage clears that copy too.
      (sci/eval-string* child "(def a nil)")
      (sci/eval-string* parent "(def a nil)")
      (let [released (released? probe parent)]
        (is released "the child does not keep the copied value"))
      (is (nil? (sci/eval-string* child "a"))))))

(deftest live-atoms-keep-their-values-through-sweeps
  (let [ctx (forkable-context)]
    (sci/eval-string* ctx "(def live (atom 42)) (def box (volatile! :v))")
    (let [child (sci/fork ctx)]
      (sci/eval-string* child "(swap! live inc)")
      (dotimes [_ 5]
        (System/gc)
        (churn! ctx 5000)
        (churn! child 5000))
      (is (= 42 (sci/eval-string* ctx "@live")))
      (is (= :v (sci/eval-string* ctx "@box")))
      (is (= 43 (sci/eval-string* child "@live"))))))

(deftest the-registry-stays-bounded-under-churn
  (let [ctx (forkable-context)]
    (dotimes [_ 20]
      (churn! ctx 5000)
      (System/gc))
    (churn! ctx 5000)
    (testing "the registry drops the descriptors of dead atoms"
      (is (< (count (:managed-descriptors @(registry ctx))) 20000)
          "100 000 dropped atoms leave a bounded registry"))))

(deftest concurrent-swaps-lose-no-update-during-sweeps
  (let [ctx (forkable-context)
        _ (sci/eval-string* ctx "(def counters (vec (repeatedly 8 #(atom 0))))")
        workers (doall
                 (for [i (range 8)]
                   (future
                     (sci/eval-string* ctx (str "(dotimes [_ 20000] (swap! (nth counters " i ") inc))")))))
        churner (future (dotimes [_ 20] (churn! ctx 5000) (System/gc)))]
    (run! deref workers)
    @churner
    (is (= (repeat 8 20000) (sci/eval-string* ctx "(mapv deref counters)")))))
