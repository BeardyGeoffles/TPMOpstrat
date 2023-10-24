       identification division.

       program-id. 201Cust is recursive.

       environment division.

       input-output section.

       file-control.

              select custfile assign "C:\gagodata\tpm\customers.dat"

              organization indexed

              access dynamic

              file status file-status

              record key cust-id OF custrec.

       configuration section.

       data division.

       file section.

       fd custfile.

       01 custrec.
         03 cust-id pic X(10).
         03 cust-name pic X(30).
         03 cust-address1 pic X(40).
         03 cust-address2 pic X(40).
         03 cust-postcode pic X(10).

       working-storage section.

       01 ws-customer-record.

         05 ws-cust-id PIC X(10) value spaces.

         05 ws-cust-name PIC X(30) value spaces.

         05 ws-cust-address1 PIC X(40) value spaces.

         05 ws-cust-address2 PIC X(40) value spaces.

         05 ws-cust-postcode PIC X(10) value spaces.

       01 ws-new-customer PIC X(1) value "N".

         88 new-customer value "Y" "y".

       01 ws-delete-customer PIC X(1) value "N".

         88 delete-customer value "Y" "y".

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

       01 customer-screen.

      *  HEADER

         03 blank screen.

         copy "screen-header.cpy".

      *  MANAGE CUSTOMER

         03 foreground-color 14 line 2 col 2 value "MANAGE CUSTOMERS".

         03 line 6 col 2 value "New customer?:    [".

         03 pic x(1) using ws-new-customer highlight prompt " ".

         03 value "]".

         03 line 8 col 2 value "Customer no:      [".

         03 pic x(10) using ws-cust-id highlight prompt " ".

         03 value "]   Leave blank if new customer".

         03 line 10 col 2 value "Name:             [".

         03 pic x(30) using ws-cust-name highlight prompt " ".

         03 value "]".

         03 line 11 col 2 value "Address 1:        [".

         03 pic x(40) using ws-cust-address1 highlight prompt " ".

         03 value "]".

         03 line 12 col 2 value "Address 2:        [".

         03 pic x(40) using ws-cust-address2 highlight prompt " ".

         03 value "]".

         03 line 13 col 2 value "Postcode:         [".

         03 pic x(10) using ws-cust-postcode highlight prompt " ".

         03 value "]".

         03 line 16 col 2 value "Delete customer?: [".

         03 pic x(1) using ws-delete-customer highlight prompt " ".

         03 value "]".

         03 line 18 col 2 value "Save changes?:    [".

         03 pic x(1) using ws-save-changes highlight prompt " ".

         03 value "]".

      *  MENU

         copy "menuprint.cpy".

         03 line 25 col 2 pic x(70) from ws-error.

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

           display customer-screen

           accept customer-screen.

      *    TODO - PROCESS MENU KEYS

           if user-option not equals space

               move "1" to ws-done

               exit

           end-if

           if not done

               if new-customer

                   move spaces to ws-cust-id

                   if ws-cust-name not equals spaces and
                     ws-cust-address1 not equals spaces and
                     ws-cust-address2 not equals spaces and
                     ws-cust-postcode not equals spaces

                       perform p230-save-new-customer

                       perform p220-blank-fields

                       string "New customer created with reference: "
                         ws-next-reference

                         into ws-error

                   else

                       move
                       "ERROR: Name, Address and Postcode are required."

                         to ws-error

                   end-if

               ELSE

                   if ws-cust-id not equals space

                       evaluate TRUE

                           when function trim (ws-cust-id) is not

                         numeric

                               move "Customer reference invalid." to
                                 ws-error

                               perform p220-blank-fields

                           when delete-customer

                               perform p260-delete-customer

                               perform p220-blank-fields

                           when save-changes

                               perform p250-update-customer

                               move "N" to ws-save-changes

                           when other

                               perform p240-read-customer

                               move "N" to ws-new-customer

                       end-evaluate

                   ELSE

                       perform p280-name-search

                   end-if

               end-if

           end-if.

           copy "get-system-date.cpy".

       p220-blank-fields.

           move "N" to ws-new-customer

           move "N" to ws-save-changes

           move "N" to ws-delete-customer

           move spaces to ws-customer-record.

       p230-save-new-customer.

           perform p270-generate-reference

           open i-o custfile

           move ws-customer-record to custrec

           write custrec

           CLOSE custfile.

       p240-read-customer.

           move "0" to invalid-data

           move function trim (ws-cust-id) to
             ws-numeric-reference

           move ws-numeric-reference to ws-cust-id

           open i-o custfile

           if file-status = "35"

               move "1" to invalid-data

           end-if

           move ws-cust-id to cust-id

           read custfile

               invalid key

                   move "1" to invalid-data.

           if not key-not-found

               move custrec to ws-customer-record

               move "Customer record retrieved." to ws-error

           else

               move "Customer record does not exist." to ws-error

               perform p220-blank-fields

           end-if.

           close custfile.

       p250-update-customer.

           open i-o custfile

           move ws-customer-record to custrec

           rewrite custrec

               invalid key

                   move "Customer record not found." to ws-error

               not invalid key

                   move "Customer record updated." to ws-error

                   CLOSE custfile.

       p260-delete-customer.

           open i-o custfile

           move ws-customer-record to custrec

           delete custfile record

               invalid key

                   move "Customer record not found." to ws-error

               not invalid key

                   move "Customer record deleted." to ws-error

                   CLOSE custfile.

       p270-generate-reference.

           move "0" to end-of-file

           open input custfile

           if file-status = "35"

               move 0 to ws-numeric-reference

           else

               perform until eof

                   read custfile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           move cust-id of custrec to ws-next-reference

               end-perform

               close custfile

               move function trim (ws-next-reference) to
                 ws-numeric-reference

           end-if.

           add 1 to ws-numeric-reference

           move ws-numeric-reference to ws-next-reference

           move ws-next-reference to ws-cust-id.

       p280-name-search.

           move "0" to end-of-file

           open input custfile

           if file-status not equal "35"

               perform until eof

                   read custfile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           if cust-name = ws-cust-name

                               move cust-id to ws-cust-id

                           end-if

               end-perform

           end-if.

           close custfile.

           if ws-cust-id not equal spaces

               perform p240-read-customer

           else

               move "Customer name not found." to ws-error

           end-if.

       p300-end.

           COPY "menuoption.cpy".

       end program 201Cust.