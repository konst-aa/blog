(ns pal-blog-generator.core
  (:gen-class)
  (:use selmer.parser)
  (:use markdown.core)
  (:require
   [clojure.core.reducers :as r]
   [clojure.string :as s]
   [clojure.data.json :as json]
   [clojure.java.io :as io]))

(defn acc-args
  ([] {})
  ([acc item]
   (if (s/starts-with? item "-")
     (conj acc {item []})
     (let [l (last acc)]
       (assoc
        acc
        (first l)
        (conj (last l) item))))))

(defn load-reducer
  ([template target]
   (let [template (slurp template)]
     (fn [history item]
       (let [templating-dict
             {:markdown (md-to-html-string (slurp item))}]
         (spit
          (str target "/" (count history) ".html")
          (render template templating-dict)))
       (conj history (count history))))))

(defn -main
  "does all the cli stuff"
  [& args]
  (let [arg-map (r/fold acc-args args)]
    (assert (-> arg-map
                (get "-template" [])
                (not= [])))
    (assert (-> arg-map
                (get "-target" [])
                (not= [])))
    (.mkdir (java.io.File. (first (get arg-map "-target"))))

    (reduce
     (load-reducer (first (get arg-map "-template"))
                   (first (get arg-map "-target")))
     (case (get arg-map "-history" [])
       [] []
       :else (json/read-str (slurp (first (get arg-map "-history")))))
     (if (contains? arg-map "-d")
       (let [d (first (get arg-map "-d"))]
         (map (fn [path] (str d path)) (.list (io/file d))))
       (get arg-map "-i")))))