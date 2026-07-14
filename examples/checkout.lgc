(module-signature ShippingPolicy
  (val free-threshold :int)
  (val standard-fee :int)
  (val label :string))

(module StandardShipping ShippingPolicy
  (def free-threshold 5000)
  (def standard-fee 799)
  (def label "standard"))

(module ExpressShipping ShippingPolicy
  (def free-threshold 8000)
  (def standard-fee 1199)
  (def label "express"))

(module-functor MakeCheckout [Policy ShippingPolicy]
  (defn shipping [subtotal]
    (if (>= subtotal Policy/free-threshold)
      0
      Policy/standard-fee))
  (defn total [subtotal]
    (+ subtotal (shipping subtotal)))
  (defn receipt [customer subtotal]
    (str
      (String.uppercase_ascii customer)
      ":"
      Policy/label
      ":"
      (total subtotal))))

(module-apply StandardCheckout MakeCheckout StandardShipping)
(module-apply ExpressCheckout MakeCheckout ExpressShipping)

(def cart {:customer "ada" :subtotal 4200})

(println (StandardCheckout/receipt (:customer cart) (:subtotal cart)))
(println (ExpressCheckout/receipt (:customer cart) (:subtotal cart)))
