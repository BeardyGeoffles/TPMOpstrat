       identification division.

       program-id. 002MainMenu is recursive.

       data division.

       working-storage section.

       01 user-option PIC x(1) value space.

       copy "system-date-variables".

       SCREEN SECTION.

       01 MENU-SCREEN.

         03 blank screen.

         copy "screen-header.cpy".

*      SCREEN TITLE

         03 foreground-color 14 VALUE "MAIN MENU" LINE 2 COL 2.

*      MENU SCREEN ITEMS

         03 VALUE "1   Manage User" LINE 7 col 10.

         03 VALUE "2   Manage Customer" LINE 8 COL 10.

         03 VALUE "3   Manage Property" LINE 9 col 10.

         03 VALUE "4   Book Viewings" LINE 10 COL 10.

         03 VALUE "5   Management Information" LINE 11 COL 10.

         03 LINE 20 COL 10 VALUE "OPTION : [".

         03 pic x(1) using user-option highlight prompt " ".

         03 VALUE "] - INPUT NUMBER AND PRESS ENTER" LINE 20 COL 21.

         03 VALUE "9 = LOG OUT" LINE 22 COL 65.

         03 FOREGROUND-COLOUR 2 VALUE
        "ALL RIGHTS RESERVED - TORRANS PROPERTY MANAGEMENT COMPANY 2023"
            LINE 25 COL 10.

       procedure division.

       main section.

           perform p001-menu

           goback.

       myproc section.

       p001-menu.

           perform p210-getsystemdate.

           display MENU-SCREEN.

           accept MENU-SCREEN
 

           Copy "menuoption.cpy".


           copy "get-system-date.cpy".

       end program 002MainMenu.