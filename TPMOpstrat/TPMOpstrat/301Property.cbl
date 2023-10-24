       identification division.

       program-id. 301Property IS recursive.

       environment division.

       input-output section.

       file-control.

              select PropertyFile assign "C:\gagodata\tpm\Properties.dat"

              organization indexed

              access dynamic

              file status file-status

              record key Property-ID OF Property-Record.

              select custfile assign "C:\gagodata\tpm\customers.dat"

              organization indexed

              access dynamic

              file status file-status

              record key cust-id OF custrec.

       configuration section.

       data division.

       file section.

       fd PropertyFile.

       01 Property-Record.

         03 Property-ID pic X(10).

         03 Customer-ID pic X(10).

         03 Property-Address1 pic X(25).

         03 Property-Address2 pic X(25).

         03 Property-Postcode pic X(10).

         03 Property-Price PIC ZZZ,ZZZ,ZZZ.ZZ.

         03 Property-Status pic x(2).

         03 Property-Info pic x(50).

         03 Sold-date pic x(14).

         03 Sold-price pic ZZZ,ZZZ,ZZZ.ZZ.

       fd custfile.

       01 custrec.

         03 cust-id pic X(10).

         03 cust-name pic X(30).

         03 filler pic X(90).

       working-storage section.

       01 ws-Property-record.

         05 ws-Property-ID PIC X(10) value spaces.

         05 ws-cust-ID PIC X(10) value spaces.

         05 ws-Property-Address1 PIC X(25) value spaces.

         05 ws-Property-Address2 PIC X(25) value spaces.

         05 ws-Property-Postcode PIC X(10) value spaces.

         05 ws-Property-Price PIC ZZZ,ZZZ,ZZZ.ZZ value spaces.

         05 WS-Property-Status PIC X(2) value spaces.

           88 valid-property-status value "FS" "fs" "SO" "so" "WD" "wd".

         05 ws-Property-Info pic x(50) value spaces.

         03 ws-Sold-date pic x(14) value spaces.

         03 ws-Sold-price pic ZZZ,ZZZ,ZZZ.ZZ.

       01 ws-cust-Name PIC X(30) value spaces.

       01 ws-new-Property PIC X(1) value "N".

         88 new-Property value "Y" "y".

       01 ws-delete-Property PIC X(1) value "N".

         88 delete-Property value "Y" "y".

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

       01 ws-valid-customer pic x(1) value "0".

         88 valid-customer value "1".

       Screen section.

       01 Property-screen.

         03 blank screen.

       COPY "screen-header.cpy".


      *  MANAGE Property

         03 FOREGROUND-COLOUR 14 line 2 col 2 value "MANAGE PROPERTIES".

         03 line 6 col 2 value "New Property?:    [".

         03 pic x(1) using ws-new-Property highlight prompt " ".

         03 value "]".

         03 line 8 col 2 value "Property no:      [".

         03 pic x(10) using ws-Property-ID highlight prompt " ".

         03 value "]   Leave blank if new Property".

         03 line 10 col 2 value "Customer no:      [".

         03 pic x(10) using ws-cust-id highlight prompt " ".

         03 value "]".

         03 line 10 col 35 value "Customer:  [".

         03 pic x(30) using ws-cust-name highlight prompt " ".

         03 value "]".

         03 line 11 col 2 value "Address 1:        [".

         03 pic x(25) using ws-Property-Address1 highlight prompt " ".

         03 value "]".

         03 line 12 col 2 value "City/Town:        [".

         03 pic x(25) using ws-Property-Address2 highlight prompt " ".

         03 value "]".

         03 line 13 col 2 value "Postcode:         [".

         03 pic x(10) using ws-Property-Postcode highlight prompt " ".

         03 value "]".

         03 line 14 col 2 value "Asking Price:     [".

         03 PIC ZZZ,ZZZ,ZZZ.ZZ using ws-Property-Price highlight prompt
            " ".

         03 value "]".

         03 line 15 col 2 value "Status:           [".

         03 PIC x(2) using ws-Property-Status highlight prompt " ".

         03 value "] - FS = For Sale  SO = Sold  WD = Withdrawn".

         03 line 16 col 2 value "Property Info:    [".

         03 PIC x(50) using ws-Property-Info highlight prompt " ".

         03 value "]".

         03 line 12 col 50 "Sold Date: [".

         03 PIC x(14) using ws-Sold-Date highlight prompt " ".

         03 value "]".

         03 line 13 col 50 "Sold Amt:  [".

         03 PIC ZZZ,ZZZ,ZZZ.ZZ using ws-Sold-Price highlight prompt " ".

         03 value "]".

         03 line 18 col 2 value "Delete Property?: [".

         03 pic x(1) using ws-delete-Property highlight prompt " ".

         03 value "]".

         03 line 19 col 2 value "Save changes?:    [".

         03 pic x(1) using ws-save-changes highlight prompt " ".

         03 value "]".

         03 line 25 col 2 pic x(70) from ws-error.

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

           display Property-screen

           accept Property-screen.

           if user-option not equals space

               move "1" to ws-done

               exit

           end-if

           if not done

               perform p502-checkvalidcustomer

               if not valid-property-status

                   move spaces to WS-Property-Status

               end-if

               if new-Property

                   move spaces to ws-Property-ID

                   if valid-customer and valid-property-status and
                     ws-Property-Address1 not equals spaces and
                     ws-Property-Address2 not equals spaces and
                     ws-Property-Postcode not equals spaces and
                     ws-Property-Price not equals spaces

                       perform p230-save-new-Property

                       perform p220-blank-fields

                       string "New Property created with reference: "
                         ws-next-reference

                         into ws-error

                   else

                       move
        "ERROR: Address, postcode, customer, status and price required."

                         to ws-error

                   end-if

               ELSE

                   if ws-Property-ID not equals space

                       evaluate TRUE

                           when function trim (ws-Property-ID) is not

                         numeric

                               move "Property reference invalid." to
                                 ws-error

                               perform p220-blank-fields

                           when delete-Property

                               perform p260-delete-Property

                               perform p220-blank-fields

                           when save-changes

                               perform p250-update-Property

                               move "N" to ws-save-changes

                           when other

                               perform p240-read-Property

                               move "N" to ws-new-Property

                       end-evaluate

                   ELSE

                       if ws-Property-Address1 not equals spaces

                           perform p280-name-search

                       end-if

                   end-if

               end-if

           end-if.

           COPY "get-system-date.cpy".

       p220-blank-fields.

           move "N" to ws-new-Property

           move "N" to ws-save-changes

           move "N" to ws-delete-Property

           move spaces to ws-Property-record

           move spaces to ws-cust-Name.

       p230-save-new-Property.

           perform p270-generate-reference

           open i-o PropertyFile

           move ws-Property-record to Property-Record

           write Property-Record

           CLOSE PropertyFile.

       p240-read-Property.

           move "0" to invalid-data

           move function trim (ws-Property-ID) to
             ws-numeric-reference

           move ws-numeric-reference to ws-Property-ID

           open i-o PropertyFile

           move ws-Property-ID to Property-ID

           read PropertyFile

               invalid key

                   move "1" to invalid-data.

           if not key-not-found

               move Property-Record to ws-Property-record

               move "Property record retrieved." to ws-error

               perform p502-checkvalidcustomer

           else

               move "Property record does not exist." to ws-error

               perform p220-blank-fields

           end-if.

           close PropertyFile.

       p250-update-Property.

           open i-o PropertyFile

           move ws-Property-record to Property-Record

           rewrite Property-Record

               invalid key

                   move "Property record not found." to ws-error

               not invalid key

                   move "Property record updated." to ws-error

                   CLOSE PropertyFile.

       p260-delete-Property.

           open i-o PropertyFile

           move ws-Property-record to Property-Record

           delete PropertyFile record

               invalid key

                   move "Property record not found." to ws-error

               not invalid key

                   move "Property record deleted." to ws-error

                   CLOSE PropertyFile.

       p270-generate-reference.

           move "0" to end-of-file

           open input PropertyFile

           if file-status = "35"

               move 0 to ws-numeric-reference

           else

               perform until eof

                   read PropertyFile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           move Property-ID of Property-Record to
                             ws-next-reference

               end-perform

               close PropertyFile

               move function trim (ws-next-reference) to
                 ws-numeric-reference

           end-if.

           add 1 to ws-numeric-reference

           move ws-numeric-reference to ws-next-reference

           move ws-next-reference to ws-Property-ID.

       p280-name-search.

           move "0" to end-of-file

           open input PropertyFile

           if file-status not equal "35"

               perform until eof

                   read PropertyFile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           if Property-Address1 = ws-property-address1

                               move Property-ID to ws-Property-ID

                           end-if

               end-perform

           end-if.

           close PropertyFile.

           if ws-Property-ID not equal spaces

               perform p240-read-Property

           else

               move "Property not found." to ws-error

           end-if.

       p300-end.

           copy "menuoption.cpy".

       p502-checkvalidcustomer.

           move "0" to ws-valid-customer.

           if ws-cust-id not equal spaces

               if function trim (ws-cust-id) is not numeric

                   move spaces to ws-cust-id

                   move spaces to ws-cust-name

               else

                   perform p512-read-customer

               end-if

           else

               if ws-cust-name not equal spaces

                   perform p522-search-customer

               end-if

           end-if.

       p512-read-customer.

           move "0" to invalid-data

           move function trim (ws-cust-id) to
             ws-numeric-reference

           move ws-numeric-reference to ws-cust-id

           open i-o custfile

           if file-status = "35"

               move "1" to invalid-data

           end-if

           move ws-cust-id to cust-id of custrec

           read custfile

               invalid key

                   move "1" to invalid-data.

           if not key-not-found

               move cust-name of custrec to ws-cust-name

               move "1" to ws-valid-customer

           else

               move "Customer record does not exist." to ws-error

               move spaces to ws-cust-id

               move spaces to ws-cust-name

               move "0" to ws-valid-customer

           end-if.

           close custfile.

       p522-search-customer.

           move "0" to end-of-file

           open input custfile

           if file-status not equal "35"

               perform until eof

                   read custfile next record

                       at end

                           move "1" to end-of-file

                       not at end

                           if cust-name = ws-cust-name

                               move cust-id of custrec to ws-cust-id

                           end-if

               end-perform

           end-if.

           close custfile.

           if ws-cust-id not equal spaces

               perform p512-read-customer

           else

               move "Customer name not found." to ws-error

           end-if.

       end program 301Property.