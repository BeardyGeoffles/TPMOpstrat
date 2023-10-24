       p210-getsystemdate.
           move function current-date to ws-system-date
           string ws-system-date(9:2)
             ":"
             ws-system-date(11:2)
             space
             ws-system-date(7:2)
             "/"
             ws-system-date(5:2)
             "/"
             ws-system-date(1:4)
             into ws-display-date.
