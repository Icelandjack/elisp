;; Keyboard macros

(fset 'mlxq
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 50 24 98 114 101 115 117 108 116 return S-down 24 98 120 113 117 101 114 121 return 134217848 120 113 117 101 114 121 32 109 111 100 101 return] 0 "%d")) arg)))

(fset 'open-vincos-5-log
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 6 1 47 115 115 104 58 118 105 110 99 111 115 45 53 58 47 backspace 25 11 return] 0 "%d")) arg)))


(fset 'dc-insert-note
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 1 return tab up tab 110 111 116 backspace backspace backspace 78 111 116 101 40 34 100 101 98 117 103 62 32 3 35 34 41 59 134217837] 0 "%d")) arg)))

