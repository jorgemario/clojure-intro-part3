(ns clojure-intro-part3.core
  (:use bakery.core))

(defn error [& rs]
  (apply println rs)
  :error)

(def scooped-ingredients #{:milk :sugar :flour :cocoa})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

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

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :butter 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn bake-brownies []
  (add :butter 2)
  (add :sugar 1)
  (add :cocoa 2)
  (mix)
  (add :flour 2)
  (add :egg 2)
  (add :milk 1)
  (mix)

  (pour-into-pan)
  (bake-pan 35)
  (cool-pan))

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
  (cond
    (= :cake item)
    (bake-cake)
    (= :cookies item)
    (bake-cookies)
    (= :brownies item)
    (bake-brownies)
    :else
    (error "Don't know how to bake" item)))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [shopping-list times]
  (into {}
        (for [[ingredient amount] shopping-list]
          [ingredient (* times amount)])))

(defn order->ingredients [order]
  (add-ingredients
   (multiply-ingredients {:butter 2 :sugar 1 :cocoa 2 :flour 2 :egg 2 :milk 1} (:brownies (:items order) 0))
   (add-ingredients
    (multiply-ingredients {:egg 2 :flour 2 :milk 1 :sugar 1} (:cake (:items order) 0))
    (multiply-ingredients {:egg 1 :flour 1 :sugar 1 :butter 1} (:cookies (:items order) 0)))))

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

