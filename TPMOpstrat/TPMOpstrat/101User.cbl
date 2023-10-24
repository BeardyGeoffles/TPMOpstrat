       identification division.

       program-id. 101User is recursive.

       environment division.

       input-output section.

       file-control.

           select userfile assign "C:\gagodata\tpm\users.dat"

           organization indexed

           access dynamic

           file status file-status

           record key user-id OF userrec.

       configuration section.

       data division.

       file section.

       fd userfile.

       01 userrec.

         03 user-id pic x(10).

         03 user-name pic x(30).

         03 user-role1 pic x(1).

         03 user-role2 pic x(1).

         03 user-role3 pic x(1).

         03 user-role4 pic x(1).

       working-storage section.

       01 ws-user-record.

         03 ws-user-id PIC X(10) value spaces.

         03 ws-user-name PIC X(30) value spaces.

         03 ws-role1 pic x(1) value space.

         03 ws-role2 pic x(1) value space.

         03 ws-role3 pic x(1) value space.

         03 ws-role4 pic x(1) value space.

       01 ws-new-user PIC X(1) value "N".

         88 new-user value "Y" "y".

       01 ws-delete-user PIC X(1) value "N".

         88 delete-user value "Y" "y".

       01 ws-save-changes PIC X(1) value "N".

         88 save-changes value "Y" "y".

       01 user-option PIC X(1) value space.

       01 ws-error pic x(70) VALUE spaces.

       copy "system-date-variables.cpy".




       01 ws-done pic x(1) value "0".

         88 done value "1".

       01 end-of-file pic x value "0".

         88 eof value "1".

       01 invalid-data pic x value "0".

         88 key-not-found value "1".

       01 file-status pic xx.

       01 ws-next-reference pic x(10) value spaces.

       01 ws-numeric-reference pic 9(10) value 0.

       Screen section.

       01 user-screen.

         03 blank screen.

           copy "screen-header".
      


      *  MANAGE USER

         03 FOREGROUND-COLOR 14 line 2 col 2 value "MANAGE USERS".

         03 line 6 col 2 value "New user?:        [".

         03 pic x(1) using ws-new-user highlight prompt " ".

         03 value "]".

         03 line 8 col 2 value "User no:          [".

         03 pic x(10) using ws-user-id highlight prompt " ".

         03 value "]   Leave blank if new user".

         03 line 10 col 2 value "Name:             [".

         03 pic x(30) using ws-user-name highlight prompt " ".

         03 value "]".

         03 line 12 col 2 value "Function (Y/N):   [".

         03 pic x(1) using ws-role1 highlight prompt " ".

         03 line 12 col 22 value "] Admin     [".

         03 pic x(1) using ws-role2 highlight prompt " ".

         03 line 12 col 35 value " ] Bookings".

         03 line 13 col 20 value "[".

         03 pic x(1) using ws-role3 highlight prompt " ".

         03 line 13 col 22 value "] Sales     [".

         03 pic x(1) using ws-role4 highlight prompt " ".

         03 line 13 col 35 value " ] Viewings".

         03 line 16 col 2 value "Delete user?:     [".

         03 pic x(1) using ws-delete-user highlight prompt " ".

         03 value "]".

         03 line 18 col 2 value "Save changes?:    [".

         03 pic x(1) using ws-save-changes highlight prompt " ".

         03 value "]".

         03 line 25 col 2 pic x(70) from ws-error.

      * menuprint

         copy "menuprint.cpy".
                  



       procedure division.

       MAIN section.

           perform p100-setup

           perform p200-process until done

           perform p300-end

           goback.

       PROCS section.

       p100-setup.

           move spaces to ws-error.

           move "0" to ws-done.

           move spaces to user-option.

       p200-process.

           perform p210-getsystemdate

           display user-screen

           accept user-screen.

           if ws-role1 = "Y" or ws-role1 = "y"

               move "Y" to ws-role1

           else

               move "N" to ws-role1

           end-if

           if ws-role2 = "Y" or ws-role2 = "y"

               move "Y" to ws-role2

           else

               move "N" to ws-role2

           end-if

           if ws-role3 = "Y" or ws-role3 = "y"

               move "Y" to ws-role3

           else

               move "N" to ws-role3

           end-if

           if ws-role4 = "Y" or ws-role4 = "y"

               move "Y" to ws-role4

           else

               move "N" to ws-role4

           end-if

           if user-option not equals space

               move "1" to ws-done

               exit

           end-if

           if not done

               if new-user

                   move spaces to ws-user-id

                   if ws-user-name not equals spaces

                       perform p230-save-new-user

                       perform p220-blank-fields

                       string "New user created with reference: "
                         ws-next-reference into ws-error

                   else

                       move
                         "ERROR: Name is required." to ws-error

                   end-if

               ELSE

                   if ws-user-id not equals space

                       evaluate TRUE

                           when function trim (ws-user-id) is not

                         numeric

                               move "User reference invalid." to
                                 ws-error

                               perform p220-blank-fields

                           when delete-user

                               perform p260-delete-user

                               perform p220-blank-fields

                           when save-changes

                               perform p250-update-user

                               move "N" to ws-save-changes

                           when other

                               perform p240-read-user

                               move "N" to ws-new-user

                       end-evaluate

                   ELSE

                       perform p280-name-search

                   end-if

               end-if

           end-if.

           copy "get-system-date.cpy".




       p220-blank-fields.

           move "N" to ws-new-user

           move "N" to ws-save-changes

           move "N" to ws-delete-user

           move spaces to ws-user-record.

       p230-save-new-user.

           perform p270-generate-reference

           open i-o USERfile

           move ws-user-record to userrec

           write userrec

           CLOSE userfile.

       p240-read-user.

           move "0" to invalid-data

           move function trim (ws-user-id) to
             ws-numeric-reference

           move ws-numeric-reference to ws-user-id

           open i-o userfile

           if file-status = "35"

               move "1" to invalid-data

           end-if

           move ws-user-id to user-id

           read userfile

               invalid key

                   move "1" to invalid-data.

           if not key-not-found

               move userrec to ws-user-record

               move "User record retrieved." to ws-error

           else

               move "User record does not exist." to ws-error

               perform p220-blank-fields

           end-if.

           close userfile.

       p250-update-user.

           open i-o userfile

           move ws-user-record to userrec

           rewrite userrec

               invalid key

                   move "User record not found." to ws-error

               not invalid key

                   move "User record updated." to ws-error

                   CLOSE userfile.

       p260-delete-user.

           open i-o userfile

           move ws-user-record to userrec

           delete userfile record

               invalid key

                   move "User record not found." to ws-error

               not invalid key

                   move "User record deleted." to ws-error

                   CLOSE userfile.

       p270-generate-reference.

           move "0" to end-of-file

           open input userfile

           if file-status = "35"

               move 0 to ws-numeric-reference

           else

               perform until eof

                   read userfile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           move user-id of userrec to ws-next-reference

               end-perform

               close userfile

               move function trim (ws-next-reference) to
                 ws-numeric-reference

           end-if.

           add 1 to ws-numeric-reference

           move ws-numeric-reference to ws-next-reference

           move ws-next-reference to ws-user-id.

       p280-name-search.

           move "0" to end-of-file

           open input userfile

           if file-status not equal "35"

               perform until eof

                   read userfile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           if user-name = ws-user-name

                               move user-id to ws-user-id

                           end-if

               end-perform

           end-if.

           close userfile.

           if ws-user-id not equal spaces

               perform p240-read-user

           else

               move "User name not found." to ws-error

           end-if.

       p300-end.

           copy "menuoption.cpy".
      


       end program 101User.