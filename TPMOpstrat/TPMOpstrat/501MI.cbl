       identification division.
       program-id. 501MI is recursive.

       environment division.
       configuration section.
       data division.
       working-storage section.
       01 user-option PIC x(1) value space.
       copy "system-date-variables.cpy".



       SCREEN SECTION.
       01 MI-SCREEN.
         03 blank screen.
         copy "screen-header.cpy".
*      SCREEN TITLE
         03 foreground-color 14 VALUE "MI MENU" LINE 2 COL 2.
*      MENU SCREEN ITEMS
         03 foreground-color 15
         VALUE "CREATE REPORT" LINE 6 col 10.
         03 VALUE "All Staff:         [ ]" LINE 8 COL 10.
         03 VALUE "Select Staff       [ ]" LINE 9 col 10.
         03 VALUE "[                    ]" LINE 9 COL 33.
         03 VALUE "Weeks              [ ]" LINE 11 COL 10.
         03 VALUE "Months             [ ]" LINE 12 COL 10.
         03 VALUE "Years              [ ]" LINE 13 COL 10.
       copy "menuprint.cpy".

       procedure division.
       main section.
           perform p001-menu
           goback.
           copy "get-system-date.cpy".

       myproc section.
           move spaces to user-option.

       p001-menu.
           move spaces to user-option.
           display MI-SCREEN.
           accept MI-SCREEN

           Copy "menuoption.cpy".

       P999-1.
       end program 501MI.
