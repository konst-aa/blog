(ns pal-blog-generator.core
  (:gen-class)
  (:use [selmer.parser]
        [markdown.core]
        [clojure.string :only (join split)])
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

(defn load-prev
  ([n history]
   (if (not (empty? history))
     (join
      "\n" (map
            (fn [item]
              (render "[{{name}}]({{link}})  " {:name (first item) :link (last item)}))
            (take-last n history)))
     "")))

(defn load-reducer
  ([template target]
   (let [template (slurp template)]
     (fn [history item]
       (let [templating-dict
             {:markdown (md-to-html-string (slurp item))
              :prev (md-to-html-string (str "##Previous articles: \n" (load-prev 5 history)))}]
         (spit
          (str target "/" (count history) ".html")
          (render template templating-dict)))
         ; my regex didn't work so now I suffer the consequences
       (let
        [new-history (conj history (list (second (reverse (split item #"(\/|\.)"))) (count history)))]
         (spit
          (str target "/archive.html")
          (render template {:markdown (md-to-html-string (str "##Archive: \n" (load-prev 100 new-history)))}))
         new-history)))))

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
     []
     (if (contains? arg-map "-d")
       (let [d (first (get arg-map "-d"))]
         (map (fn [path] (str d path)) (.list (io/file d))))
       (get arg-map "-i")))))