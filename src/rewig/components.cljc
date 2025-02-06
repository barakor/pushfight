(ns rewig.components
  (:require 
            ; [reagent.core :as r :refer [with-let]]
            [rewig.util :refer [size-map named-sides-map]]
            [rewig.theme.gruvbox :as theme]))

(defn box 
  ([children]
   (box {} children))
  ([{:keys [element 
            align content-align wrap direction display 
            hidden? reverse?
            size padding margin 
            click!
            attrs
            css] :as props} children]
   (let [
         element       (or element :div)
         attrs         (merge (or attrs {}) 
                              {:on-click click!}) 
                               

         hidden?       (or hidden? false)
         reverse?      (or reverse? false)

         display       (name (or display :flex))
         align         (name (or align :flex-start))
         content-align (name (or content-align :flex-start))
         wrap          (name (or wrap :nowrap))
         direction     (str (name (or direction :row)) (when reverse? "-reverse"))


         css           (merge css
                              (size-map size)
                              (named-sides-map "margin" margin)
                              (named-sides-map "padding" padding)

                              {:display display
                               :align-items align
                               :justify-content content-align
                               :flex-wrap wrap
                               :flex-direction direction})

         props (assoc attrs :style (merge (:style attrs) css))
         
         children (cond 
                    (fn? (first children )) [children]
                    (keyword? (first children )) [children]
                    :else children)]

     (into [element props] children)))) 
      

(defn row
 ([children]
  (row {} children))
 ([props children]
  (let [props (assoc props :direction :row)]
    (box props children))))


(defn column
 ([children]
  (column {} children))
 ([props children]
  (let [props (assoc props :direction :column)]
    (box props children))))


(defn gap [& {:keys [size flex] :or {size 0}}]
  (box {:css {:flex-grow flex :flex-shrink 0} :size size} nil))


(defn button 
  ([children]
   (button {} children))
  ([{:keys [disabled? click! attrs type css] :as props} children]
   (let [attrs (merge attrs 
                       {:disabled disabled?})
         type (or type :normal)
         css (merge {:cursor "pointer"
                     :border "none"}
                    (case type
                      :normal  {:background-color theme/bg0
                                :color theme/fg0}
                                
                      :primary {:background-color theme/primary
                                :color theme/fg0}
                                
                      :success {:background-color theme/success
                                :color theme/fg0}
                                
                      :danger  {:background-color theme/danger
                                :color theme/fg0})
                                

                    (when disabled?
                      {:color theme/fg4
                       :cursor "not-allowed"
                       :opacity "0.25"})
                    css)


         props (assoc props :element :button
                            :attrs attrs
                            :css css)]
     (box props children))))
