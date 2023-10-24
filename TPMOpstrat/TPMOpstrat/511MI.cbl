       identification division.
       program-id. 511MI.
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
         03 foreground-color 14 VALUE "REPORT SCREEN" LINE 2 COL 2.
*      MENU SCREEN ITEMS
         03 VALUE "REPORT OUTPUT" LINE 6 col 10.
         03 VALUE "Staff Name:        [ ]" LINE 8 COL 10.

         03 VALUE "Weeks              [ ]" LINE 10 COL 10.
         03 VALUE "Months             [ ]" LINE 11 COL 10.
         03 VALUE "Years              [ ]" LINE 12 COL 10.
         03 VALUE "Total Sales        [999,999,999.00]" LINE 14 COL 10.
         03 VALUE "Average Sales      [999,999,999.00]" LINE 15 COL 10.
         03 VALUE "Highest            [999,999,999.00]" LINE 16 COL 10.
         03 VALUE "Lowest             [999,999,999.00]" LINE 17 COL 10.

       copy "menuprint.cpy".

       procedure division.
       main section.
           perform p001-menu
           goback.

       myproc section.

       p001-menu.
           display MI-SCREEN.
           accept MI-SCREEN

           Copy "menuoption.cpy".


       P999-1.
       end program 511MI.