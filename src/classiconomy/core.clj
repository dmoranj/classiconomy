(ns classiconomy.core)

(def RESOURCES [:food :tools :clothes])
(def INITIAL-NEEDS {:food 500 :tools 100 :clothes 200})
(def INITIAL-SAVINGS {:food 2000 :tools 200 :clothes 1000 :money 4000})
(def PRODUCTIONS {:food 800 :tools 400 :clothes 600})

(defn safe-int[n]
  (if (= n nil)
    0
    (int n)))

(defn get-initial[initial minimum]
  (let [variable (- 1 minimum)]
    (apply hash-map (flatten (map #(list (first %) (int (* (+ minimum (* variable (Math/random))) (second %)))) initial)))))

(defn get-productions[]
  (let [resource (nth RESOURCES (int (* (Math/random) (count RESOURCES))))
        base-value (resource PRODUCTIONS)
        value (int (* base-value (+ 0.7 (* 0.3 (Math/random)))))]
    { resource value } ))

(defn create-individual
  "Create a random individual for the simulation."
  []
  {:needs (get-initial INITIAL-NEEDS 0.5)
   :savings (get-initial INITIAL-SAVINGS 0.5)
   :produces (get-initial (get-productions) 0)
   :reproduction 0})

(defn reduce-resource[resource individual]
  "Reduce the amount of the selected resource from each of the individual supplies"
  (let [
         current-res (get-in individual [:savings resource])
         res-need (get-in individual [:needs resource])]
    (assoc-in individual [:savings resource] (- current-res res-need))))

(defn add-resource[resource individual]
  "Add the amount of produced selected resource to the savings deposit"
  (let [
         current-res (get-in individual [:savings resource])
         res-produced (get-in individual [:produces resource])]
    (assoc-in individual [:savings resource] (+ current-res res-produced))))

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
  [individuals]
  {
    :offer (get-accumulate individuals :produces)
    :demand (get-accumulate individuals :needs)
    :savings (get-accumulate individuals :savings)
    :population (get-population individuals)
    })

(defn update-price[analysis prices resource]
  (let [
         offer (safe-int (-> analysis :offer resource))
         demand (safe-int (-> analysis :demand resource))
         price (safe-int (resource prices))
         ]
    (if (not= offer 0)
      [resource (int (* (/ demand offer) price))]
      [resource 0]
      )))

(defn update-prices[analysis prices]
  (apply hash-map (flatten (map (partial update-price analysis prices) RESOURCES))))

(defn advance-state[state]
  "Apply all the operations of a single step over each of the individuals"
  (let [
         individuals (:individuals state)
         analysis (analyse-state individuals)
         ]
  {
    :individuals (map #(-> % add-resources reduce-resources) individuals)
    :data analysis
    :prices (update-prices analysis (:prices state))
  }))

(defn calculate-initial-price[analysis resource]
  (let [
         offer (safe-int (-> analysis :offer resource))
         demand (safe-int (-> analysis :demand resource))
         money-savings (safe-int (-> analysis :savings :money))
         money (/ money-savings (count RESOURCES))
         ]
    (if (and (not= demand 0) (not= offer 0))
      [resource (int (* (/ demand offer) (/ money offer))) ]
      [resource 0]
      )))

(defn initial-prices[analysis]
  (apply hash-map (flatten (map (partial calculate-initial-price analysis) RESOURCES))))

(defn create-initial-state
  "Create an initial state for the simulation"
  [n]
  (let [
         initial-individuals (repeatedly n create-individual)
         analysis (analyse-state initial-individuals)
         ]
    {
      :individuals initial-individuals
      :data analysis
      :prices (initial-prices analysis)
      }))

(defn gen-simulation
  "Generates a lazy sequence of simulation steps based on the initial state i"
  [i]
  (lazy-seq (cons i (gen-simulation (advance-state i)))))

(defn run-simulation
  "Run a simulation with n individuals for t iterations"
  [n t]
  (take t (gen-simulation (create-initial-state n))))
