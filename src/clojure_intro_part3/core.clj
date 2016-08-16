(ns clojure-intro-part3.core
  (:use bakery.core))

(def baking {:recipes {:cake {:ingredients {:egg 2 :flour 2 :milk 1 :sugar 1}
                              :steps [[:add :all]
                                      [:mix]
                                      [:pour]
                                      [:bake 25]
                                      [:cool]]}
                       :cookies {:ingredients {:egg 1 :flour 1 :butter 1 :sugar 1}
                                 :steps [[:add :all]
                                         [:mix]
                                         [:pour]
                                         [:bake 30]
                                         [:cool]]}
                       :brownies {:ingredients {:butter 2 :sugar 1 :cocoa 2 :flour 2 :egg 2 :milk 1}
                                  :steps [[:add :butter]
                                          [:add :sugar]
                                          [:add :cocoa]
                                          [:mix]
                                          [:add :flour]
                                          [:add :egg]
                                          [:add :milk]
                                          [:mix]
                                          [:pour]
                                          [:bake 35]
                                          [:cool]]}}
             :ingredients {:egg {:storage :fridge
                                 :usage :squeezed}
                           :milk {:storage :fridge
                                  :usage :scooped}
                           :butter {:storage :fridge
                                    :usage :simple}
                           :flour {:storage :pantry
                                   :usage :scooped}
                           :sugar {:storage :pantry
                                   :usage :scooped}
                           :cocoa {:storage :pantry
                                   :usage :scooped}}})

(defn error [& rs]
  (apply println rs)
  :error)

(defn scooped? [ingredient]
  (= :scooped (:usage (ingredient (:ingredients baking)))))

(defn squeezed? [ingredient]
  (= :squeezed (:usage (ingredient (:ingredients baking)))))

(defn simple? [ingredient]
  (= :simple (:usage (ingredient (:ingredients baking)))))

(defn add-squeezed
  ([ingredient]
     (add-squeezed ingredient 1))
  ([ingredient amount]
     (if (squeezed? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       (error "This function only works on squeezed ingredients. You asked me to squeeze" ingredient))))

(defn add-scooped
  ([ingredient]
     (add-scooped ingredient 1))
  ([ingredient amount]
     (if (scooped? ingredient)
       (do
         (grab :cup)
         (dotimes [i amount]
          (scoop ingredient)
          (add-to-bowl))
         (release))
       (error "This function only works on scooped ingredients. You asked me to scoop" ingredient))))

(defn add-simple
  ([ingredient]
     (add-simple ingredient 1))
  ([ingredient amount]
     (if (simple? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl))
       (error "This function only works on simple ingredients. You asked me to add" ingredient))))

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
     (cond
      (squeezed? ingredient)
      (add-squeezed ingredient amount)

      (simple? ingredient)
      (add-simple ingredient amount)

      (scooped? ingredient)
      (add-scooped ingredient amount)

      :else
      (error "I do not have the ingredient" ingredient))))

(def pantry-ingredients #{:sugar :flour :cocoa})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def fridge-ingredients #{:egg :milk :butter})

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error ingredient " not in pantry area"))))

(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error ingredient " not in fridge area"))))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (cond
     (from-fridge? ingredient)
     (fetch-from-fridge ingredient amount)
     
     (from-pantry? ingredient)
     (fetch-from-pantry ingredient amount)

     :else
     (error "Unable to fetch ingredient" ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-list [shopping-list]
  (doseq [[location ingredients] {:pantry pantry-ingredients
                                 :fridge fridge-ingredients}]
    (go-to location)
    (doseq [ingredient ingredients]
      (load-up-amount ingredient (ingredient shopping-list 0))))
 
  (go-to :prep-area)
  (doseq [[ingredient amount] shopping-list]
    (unload-amount ingredient amount)))

(defn bake [item]
  (bake-recipe (item (:recipes baking))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [shopping-list times]
  (into {}
        (for [[ingredient amount] shopping-list]
          [ingredient (* times amount)])))

(defn order->ingredients [order]
  (reduce add-ingredients (for [[item quantity] (:items order)]
                            (multiply-ingredients (:ingredients (item (:recipes baking))) quantity))))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredient-list (orders->ingredients orders)]
    
    (fetch-list ingredient-list)
    
    (doseq [order orders]
      (let [items (:items order)
            racks (for [[item amount] items
                        i (range amount)]
                    (bake item))
            receipt {:orderid (:orderid order)
                     :address (:address order)
                     :rackids racks}]
        (delivery receipt)))))

(defn perform [recipe step]
  (cond
    (= :cool (first step))
    (cool-pan)
    (= :mix (first step))
    (mix)
    (= :pour (first step))
    (pour-into-pan)
    (= :bake (first step))
    (bake-pan (second step))
    (= :add (first step))
    (cond
      (= 2 (count (rest step)))
      (apply add (rest step))
      (contains? (:ingredients recipe) (second step))
      (add (second step) ((second step) (:ingredients recipe)))
      (= [:all] (rest step))
      (doseq [[ingredient amount] (:ingredients recipe)]
        (add ingredient amount))
      :else
      (error "Unable to perform the add step" step))
    :else
    (error "I do not know how to" (first step))))

(defn bake-recipe [recipe]
  (last (for [step (:steps recipe)]
          (perform recipe step))))

(defn -main
  [& args]
  (day-at-the-bakery))

