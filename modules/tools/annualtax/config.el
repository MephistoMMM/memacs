;;; tools/annualtax/config.el -*- lexical-binding: t; -*-

(setq yml-data (yamlmod-read-file (expand-file-name "~/Downloads/my_income.yml")))

(defconst +annualtax--threshold 5000
  "The threshold to tax.")

(defconst +annualtax--month-name-order-map
  `((January . 1)
    (Jan . 1)
    (February . 2)
    (Feb . 2)
    (March . 3)
    (Mar . 3)
    (April . 4)
    (Apr . 4)
    (May . 5)
    (June . 6)
    (Jun . 6)
    (July . 7)
    (Jul . 7)
    (August . 8)
    (Aug . 8)
    (September . 9)
    (Sep . 9)
    (October . 10)
    (Oct . 10)
    (November . 11)
    (Nov . 11)
    (December . 12)
    (Dec . 12))
  "Map for Month name and its order.")

(defconst +annualtax--tax-rate-hierarchy
  '((0 . 0.03)
    (36000 . 0.1)
    (144000 . 0.2)
    (300000 . 0.25)
    (420000 . 0.3)
    (660000 . 0.35)
    (960000 . 0.45))
  "Dictory for tax rate.")

(defun +annualtax--month-name-to-order (name)
  "Map name to order via `month-name-order-map'."
  (let* ((name-symbol (intern name))
         (mo-map (assoc name-symbol +annualtax--month-name-order-map)))
    (if (null mo-map)
        13
      (cdr mo-map))))

(defun +annualtax--income-seq (data-hash-table)
  "Get income list and sort it, invalid data will be put to tail."
  (seq-sort-by (lambda (income)
                 (+annualtax--month-name-to-order (gethash "month" income)))
               #'< (gethash "income" data-hash-table))
  )

(defun +annualtax--accumulated-income (incomes month)
  "Calculate the accumulated income from 1 to `month'."
  (let ((end-month (- month 1)))
    (cl-loop for i from 0 to end-month
             sum (+annualtax--salary-sum
                  (aref incomes i))))
  )


(defun +annualtax--taxable-income-of-sum (sum month &rest tax-reduction)
  "Calculate the tax of sum according to tax rate directory."
  (let* ((rate 0)
         (base-reduction (* +annualtax--threshold month))
         (reduction (cond
                     ((listp tax-reduction)
                      (seq-reduce #'+ tax-reduction 0))
                     ((numberp tax-reduction)
                      (tax-reduction))
                     (t 0)))
         (taxable-income (- sum base-reduction reduction)))
    (if (<= taxable-income 0)
        0
      taxable-income))
  )

(defun +annualtax--tax-of-sum (sum month &rest tax-reduction)
  "Calculate the tax of sum according to tax rate directory."
  (let* ((rate 0)
         (taxable-income (apply #'+annualtax--taxable-income-of-sum
                          sum month tax-reduction)))
    (cl-loop
     for tuple in +annualtax--tax-rate-hierarchy
     if (<= (car tuple) taxable-income)
     do (setq rate (cdr tuple))
     else return
     (* taxable-income rate)
     end)
    )
  )

(defun +annualtax--total-tax (incomes month &rest tax-reduction)
  "Calculate payed tax."
  (apply #'+annualtax--tax-of-sum
   (+annualtax--accumulated-income incomes month)
   month
   tax-reduction)
  )

(defun +annualtax--payed-tax (incomes month &rest tax-reduction)
  "Calculate payed tax."
  (let ((previous-month (- month 1)))
    (if (< previous-month 1)
        0
      (apply #'+annualtax--total-tax incomes previous-month tax-reduction)))
  )

(defun +annualtax--salary-sum (income)
  "Calculate sum of salary."
  (seq-reduce '+ (gethash "salary" income) 0))

(let* ((incomes (+annualtax--income-seq yml-data))
      (month 12)
      (tax-reduction (* month 1500))
      (pre-tax-reduction (* (- month 1) 1500))
      (sum (+annualtax--accumulated-income incomes month)))
  (format-message "List:
income: %f,
taxable income: %f,
total tax: %f,
payed tax: %f,
current month tax: %f"
                  sum
                  (+annualtax--taxable-income-of-sum sum month tax-reduction)
                  (+annualtax--total-tax incomes month tax-reduction)
                  (+annualtax--payed-tax incomes month pre-tax-reduction)
                  (- (+annualtax--total-tax incomes month tax-reduction)
                     (+annualtax--payed-tax incomes month pre-tax-reduction))
    )
  )
