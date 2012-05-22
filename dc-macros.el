;; Keyboard macros

(fset 'mlxq
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 50 24 98 114 101 115 117 108 116 return S-down 24 98 120 113 117 101 114 121 return 134217848 120 113 117 101 114 121 32 109 111 100 101 return] 0 "%d")) arg)))
