       identification division.
       program-id. 001Login is recursive.

       data division.
       working-storage section.

       01 ws-user-name pic x(10) VALUE spaces.
       01 ws-user-password pic x(10) VALUE spaces.
       01 ws-valid-user pic x VALUE "0".
         88 is-valid-user value "1".
       01 ws-error pic x(40) VALUE spaces.

       copy "system-date-variables.cpy".

       screen section.

       01 main-screen.
         03 blank screen.

         copy "screen-header.cpy".

         03 line 10 col 29 VALUE "Username: [".
         03 pic x(10) using ws-user-name highlight prompt " ".
         03 value "]".

         03 line 12 col 29 VALUE "Password: [".
         03 pic x(10) using ws-user-password no-echo " ".
         03 value "]".

         03 line 18 col 20 pic x(40) from ws-error.

         03 FOREGROUND-COLOUR 2 VALUE
        "ALL RIGHTS RESERVED - TORRANS PROPERTY MANAGEMENT COMPANY 2023"
            LINE 25 COL 10.

       procedure division.

       MAIN section.

           perform p100-setup
           perform p200-process until is-valid-user
           perform p300-end
           goback.

       PROCS section.

       p100-setup.
           move "0" to ws-valid-user.

       p200-process.
           perform p210-getsystemdate
           display main-screen
           accept main-screen
           if ws-user-name equals "admin" and
             ws-user-password equals "password"
               move "1" to ws-valid-user
           else
               move spaces to ws-user-name
               move spaces to ws-user-password
               move "Invalid login details. Please try again."
                 to ws-error
           end-if.

           copy "get-system-date.cpy".

       p300-end.
           move spaces to ws-user-password
           move spaces to ws-error
           move "0" to ws-valid-user
           call "002MainMenu".
           goback.

       end program 001Login.