(module cuauv-dshm (name->id
                    name->type
                    name->getter
                    name->setter
                    )
  (import scheme chicken)
  (use srfi-69)
  (import cuauv-shm)

  (define registry (make-hash-table))

  <!--(for tn, t in sorted(tvmap.items()))-->
      <!--(for v in t)-->
  (hash-table-set! registry '$!srename(v[1], v[2])!$
                   (vector $!v[3]!$
                           '$!tn!$
                           $!srename(v[1], v[2])!$-ref
                           $!srename(v[1], v[2])!$-set!))
      <!--(end)-->
  <!--(end)-->

  (define (name->id name)
    (and (hash-table-exists? registry name)
         (vector-ref (hash-table-ref registry name) 0)))

  (define (name->type name)
    (and (hash-table-exists? registry name)
         (vector-ref (hash-table-ref registry name) 1)))

  (define (name->getter name)
    (and (hash-table-exists? registry name)
         (vector-ref (hash-table-ref registry name) 2)))

  (define (name->setter name)
    (and (hash-table-exists? registry name)
         (vector-ref (hash-table-ref registry name) 3)))
)
