(ns classiconomy.core)

(def RESOURCES [:food :tools :clothes])
(def INITIAL-NEEDS {:food 50 :tools 10 :clothes 15})
(def INITIAL-SAVINGS {:food 200 :tools 20 :clothes 100 :money 400})
(def PRODUCTIONS {:food 80 :tools 20 :clothes 40})

(defn get-initial[initial minimum]
  (let [variable (- 1 minimum)]
    (apply hash-map (flatten (map #(list (first %) (int (* (+ minimum (* variable (Math/random))) (second %)))) initial)))))

(defn get-productions[]
  (let [resource (nth RESOURCES (int (* (Math/random) (count RESOURCES))))
        base-value (resource PRODUCTIONS)
        value (int (* base-value (+ 0.5 (* 0.5 (Math/random)))))]
    { resource value } ))

(defn create-individual
  "Create a random individual for the simulation."
  []
  {:needs (get-initial INITIAL-NEEDS 0.5)
   :savings (get-initial INITIAL-SAVINGS 0.5)
   :produces (get-initial (get-productions) 0)
   :reproduction 0})

(defn create-initial-state
  "Create an initial state for the simulation"
  [n]
  {
    :individuals (repeatedly n create-individual)
    :data '()
    }
  )

(defn reduce-resource[resource individual]
  "Reduce the amount of the selected resource from each of the individual supplies"
  (let [
         current-food (get-in individual [:savings resource])
         food-need (get-in individual [:needs resource])]
    (assoc-in individual [:savings resource] (- current-food food-need))))

(defn add-resource[resource individual]
  "Add the amount of produced selected resource to the savings deposit"
  (let [
         current-food (get-in individual [:savings resource])
         food-produced (get-in individual [:produces resource])]
    (assoc-in individual [:savings resource] (+ current-food food-produced))))

(defn add-resources[individual]
  "Add all the productions of each resource to the corresponding savings for an individual"
  [individual]
  (let [resources (keys (:produces individual))
        productors (map #(partial add-resource %) resources)]
      (reduce #(%2 %1) individual productors)))

(defn reduce-resources[individual]
  "Reduce the resources based on the needs for an individual"
  [individual]
  (let [resources (keys (:needs individual))
        reductors (map #(partial reduce-resource %) resources)]
      (reduce #(%2 %1) individual reductors)))

(defn gen-simulation
  "Generates a lazy sequence of simulation steps based on the initial state i"
  [i]
  (lazy-seq (cons i (gen-simulation (advance-state i)))))

(defn get-population
  "Get the total population of the simulation"
  [state]
  (count state))

(defn get-accumulate
  "Get the offer for the different products (including money)"
  [state field]
  (reduce #(merge-with + %1 (field %2)) {} state))

(defn analyse-state
  "Generates a set of metrics about the current state of the simulation"
  [state]
  (let [individuals (:individuals state)]
    {
    :offer (get-accumulate individuals :produces)
    :demand (get-accumulate individuals :needs)
    :savings (get-accumulate individuals :savings)
    :population (get-population individuals)
    }))

(defn advance-state[state]
  "Apply all the operations of a single step over each of the individuals"
  {
    :individuals (map #(-> % add-resources reduce-resources) (:individuals state))
    :data (analyse-state state)
  })

(defn run-simulation
  "Run a simulation with n individuals for t iterations"
  [n t]
  (take t (gen-simulation (create-initial-state n))))
