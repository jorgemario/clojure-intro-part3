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

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release :cup))})

(defn error [& rs]
  (apply println rs)
  :error)

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
   (let [u (:usage (ingredient (:ingredients baking)))
         f (usage u)]
     (f ingredient amount))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (go-to (:storage (ingredient (:ingredients baking))))
   (load-up-amount ingredient amount)
   (go-to :prep-area)
   (unload-amount ingredient amount)))

(defn fetch-list [shopping-list]
  (let [with-storage (for [[ingredient amount] shopping-list]
                       {:name ingredient
                        :amount amount
                        :storage (:storage (ingredient (:ingredients baking)))})]
    (doseq [[location ingredients] (group-by :storage with-storage)]
      (go-to location)
      (doseq [ingredient ingredients]
        (load-up-amount (:name ingredient) (:amount ingredient))))
    
    (go-to :prep-area)
      (doseq [[ingredient amount] shopping-list]
        (unload-amount ingredient amount))))

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

(defn -main
  [& args]
  (day-at-the-bakery))

