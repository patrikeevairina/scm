(define display_trig
    (
        lambda ()
        (
            let disp_trig_rec ((start 0)) 
            (
                if (= start 180) `quit
                (
                    begin
                    (format #t "~4a   ~4f   ~4f   ~4f\n"  start (cos start) (sin start) (tan start))
                    (disp_trig_rec (+ start 5))
                )
            )
        )
    )
)

(format #t "~4a  ~4a  ~4a  ~4a\n" "rad" "cos" "sin" "tan")
(display_trig)