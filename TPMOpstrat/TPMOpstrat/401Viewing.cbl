       identification division.
       program-id. 401Viewing is recursive.
       environment division.
       input-output section.

       file-control.
              select BookingFile assign "C:\gagodata\tpm\Bookings.dat"
              organization indexed
              access dynamic
              file status file-status
              record key Booking-ID OF Booking-Record.

              select userfile assign "C:\gagodata\tpm\users.dat"
              organization indexed
              access dynamic
              file status file-status
              record key user-id OF userrec.

              select custfile assign "C:\gagodata\tpm\customers.dat"
              organization indexed
              access dynamic
              file status file-status
              record key cust-id OF custrec.

              select PropertyFile assign 
              "C:\gagodata\tpm\Properties.dat"
              organization indexed
              access dynamic
              file status file-status
              record key Property-ID OF Property-Record.

       configuration section.

       data division.

       file section.

       fd BookingFile.
       01 Booking-Record.
         03 Booking-ID pic X(10).
         03 Booking-Date.
           05 booking-day pic X(2).
           05 booking-month pic X(2).
           05 booking-year pic X(4).
         03 Booking-Time.
           05 booking-hour pic X(2).
           05 booking-mins pic X(2).
         03 Property-ID pic X(10).
         03 Customer-ID pic x(10).
         03 User-ID pic x(10).

       fd userfile.
       01 userrec.
         03 user-id pic x(10).
         03 user-name pic x(30).
         03 filler pic x(4).

       fd custfile.
       01 custrec.
         03 cust-id pic X(10).
         03 cust-name pic X(30).
         03 filler pic X(90).

       fd PropertyFile.
       01 Property-Record.
         03 Property-ID pic X(10).
         03 Customer-ID pic X(10).
         03 Property-Address1 pic X(25).
         03 filler pic x(129).

       working-storage section.

       01 ws-Booking-Record.
         05 ws-Booking-ID PIC X(10) value spaces.
         05 ws-Booking-date.
           07 ws-booking-day pic X(2) value spaces.
           07 ws-booking-month pic x(2) value spaces.
           07 ws-booking-year pic x(4) value spaces.
         05 ws-booking-time.
           07 ws-booking-hour pic X(2) value spaces.
           07 ws-booking-mins pic x(2) value spaces.
         05 ws-property-id pic x(10) value spaces.
         05 ws-cust-id pic x(10) value spaces.
         05 ws-user-id pic X(10) value spaces.
       01 ws-property-name pic x(30) value spaces.
       01 ws-cust-name pic x(30) value spaces.
       01 ws-user-name pic x(30) value spaces.
       01 ws-new-Booking PIC X(1) value "N".
         88 new-Booking value "Y" "y".
       01 ws-delete-Booking PIC X(1) value "N".
         88 delete-Booking value "Y" "y".
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
       01 ws-valid-property pic x(1) value "0".
         88 valid-property value "1".
       01 ws-valid-user pic x(1) value "0".
         88 valid-user value "1".

       Screen section.

       01 Booking-screen.
         03 blank screen.

         copy "screen-header.cpy".

      *  MANAGE Booking
         03 foreground-color 14 line 2 col 2 value "MANAGE BOOKINGS".
         03 line 6 col 2 value "New Booking?:     [".
         03 pic x(1) using ws-new-Booking highlight prompt " ".
         03 value "]".
         03 line 8 col 2 value "Booking no:       [".
         03 pic x(10) using ws-Booking-ID highlight prompt " ".
         03 value "]   Leave blank if new Booking".
         03 line 10 col 2 value "Date:             [".
         03 pic x(2) using ws-booking-day highlight prompt " ".
         03 value "/".
         03 pic x(2) using ws-booking-month highlight prompt " ".
         03 value "/".
         03 pic x(4) using ws-booking-year highlight prompt " ".
         03 value "]".
         03 line 10 col 35 value "Time:      [".
         03 pic x(2) using ws-booking-hour highlight prompt " ".
         03 value ":".
         03 pic x(2) using ws-booking-mins highlight prompt " ".
         03 value "]".
         03 line 12 col 2 value "Property no:      [".
         03 pic x(10) using ws-property-id highlight prompt " ".
         03 value "]".
         03 line 12 col 35 value "Address:   [".
         03 pic x(30) using ws-property-name highlight prompt " ".
         03 value "]".
         03 line 13 col 2 value "Customer no:      [".
         03 pic x(10) using ws-cust-id highlight prompt " ".
         03 value "]".
         03 line 13 col 35 value "Customer:  [".
         03 pic x(30) using ws-cust-name highlight prompt " ".
         03 value "]".
         03 line 14 col 2 value "User no:          [".
         03 pic x(10) using ws-user-id highlight prompt " ".
         03 value "]".
         03 line 14 col 35 value "Agent:     [".
         03 pic x(30) using ws-user-name highlight prompt " ".
         03 value "]".
         03 line 16 col 2 value "Delete Booking?:  [".
         03 pic x(1) using ws-delete-Booking highlight prompt " ".
         03 value "]".
         03 line 18 col 2 value "Save changes?:    [".
         03 pic x(1) using ws-save-changes highlight prompt " ".
         03 value "]".
         03 line 25 col 2 pic x(70) from ws-error.

         copy "menuprint.cpy".

      * menuprint

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
           display Booking-screen
           accept Booking-screen.
           if user-option not equals space
               move "1" to ws-done
               exit
           end-if
           if not done
               perform p501-checkvalidproperty
               perform p502-checkvalidcustomer
               perform p503-checkvaliduser
               if new-Booking
                   move spaces to ws-Booking-ID
                   if ws-Booking-date not equals spaces and
                     ws-Booking-time not equals spaces and
                     valid-customer and valid-property and
                     valid-user
                       perform p230-save-new-Booking
                       perform p220-blank-fields
                       string "New Booking created with reference: "
                         ws-next-reference
                         into ws-error
                   else
                       move
                         "ERROR: Date, time and references required."
                         to ws-error
                   end-if
               ELSE
                   if ws-Booking-ID not equals space
                       evaluate TRUE
                           when function trim (ws-Booking-ID) is not
                         numeric
                               move "Booking reference invalid." to
                                 ws-error
                               perform p220-blank-fields
                           when delete-Booking
                               perform p260-delete-Booking
                               perform p220-blank-fields
                           when save-changes
                               perform p250-update-Booking
                               move "N" to ws-save-changes
                           when other
                               perform p240-read-Booking
                               move "N" to ws-new-Booking
                       end-evaluate
                   end-if
               end-if
           end-if.

           copy "get-system-date.cpy".

       p220-blank-fields.
           move "N" to ws-new-Booking
           move "N" to ws-save-changes
           move "N" to ws-delete-Booking
           move spaces to ws-Booking-Record.
           move spaces to ws-user-name.
           move spaces to ws-property-name.
           move spaces to ws-cust-name.

       p230-save-new-Booking.
           perform p270-generate-reference
           open i-o BookingFile
           move ws-Booking-Record to Booking-Record
           write Booking-Record
           CLOSE BookingFile.

       p240-read-Booking.
           move "0" to invalid-data
           move function trim (ws-Booking-ID) to
             ws-numeric-reference
           move ws-numeric-reference to ws-Booking-ID
           open i-o BookingFile
           move ws-Booking-ID to Booking-ID

           read BookingFile

               invalid key
                   move "1" to invalid-data.

           if not key-not-found
               move Booking-Record to ws-Booking-Record
               move "Booking record retrieved." to ws-error
               perform p501-checkvalidproperty
               perform p502-checkvalidcustomer
               perform p503-checkvaliduser

           else

               move "Booking record does not exist." to ws-error
               perform p220-blank-fields

           end-if.
           close BookingFile.

       p250-update-Booking.
           open i-o BookingFile
           move ws-Booking-Record to Booking-Record
           rewrite Booking-Record
               invalid key
                   move "Booking record not found." to ws-error
               not invalid key
                   move "Booking record updated." to ws-error
                   CLOSE BookingFile.

       p260-delete-Booking.
           open i-o BookingFile
           move ws-Booking-Record to Booking-Record
           delete BookingFile record
               invalid key
                   move "Booking record not found." to ws-error
               not invalid key
                   move "Booking record deleted." to ws-error
                   CLOSE BookingFile.

       p270-generate-reference.
           move "0" to end-of-file
           open input BookingFile
           if file-status = "35"
               move 0 to ws-numeric-reference
           else
               perform until eof
                   read BookingFile next record
                       at end
                           move "1" to end-of-file
                       not at end
                           move Booking-ID of Booking-Record to
                             ws-next-reference
               end-perform
               close BookingFile

               move function trim (ws-next-reference) to
                 ws-numeric-reference
           end-if.
           add 1 to ws-numeric-reference
           move ws-numeric-reference to ws-next-reference
           move ws-next-reference to ws-Booking-ID.

       p300-end.

           copy "menuoption.cpy".

      * menuoption

       p501-checkvalidproperty.
           move "0" to ws-valid-property.
           if ws-property-id not equal spaces
               if function trim (ws-property-id) is not numeric
                   move spaces to ws-property-id
                   move spaces to ws-property-name
               else
                   perform p511-read-property
               end-if
           else
               if ws-property-name not equal spaces
                   perform p521-search-property
               end-if
           end-if.

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

       p503-checkvaliduser.
           move "0" to ws-valid-user.
           if ws-user-id not equal spaces
               if function trim (ws-user-id) is not numeric
                   move spaces to ws-user-id
                   move spaces to ws-user-name
               else
                   perform p513-read-user
               end-if
           else
               if ws-user-name not equal spaces
                   perform p523-search-user
               end-if
           end-if.

       p511-read-property.
           move "0" to invalid-data
           move function trim (ws-property-id) to
             ws-numeric-reference
           move ws-numeric-reference to ws-property-id
           open i-o propertyfile
           if file-status = "35"
               move "1" to invalid-data
           end-if

           move ws-property-id to property-id of Property-Record
           read propertyfile
               invalid key
                   move "1" to invalid-data.
           if not key-not-found
               move Property-Address1 of Property-Record
                 to ws-property-name
               move "1" to ws-valid-property
           else
               move "Property record does not exist." to ws-error
               move spaces to ws-property-id
               move spaces to ws-property-name
               move "0" to ws-valid-property
           end-if.

           close propertyfile.

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

       p513-read-user.
           move "0" to invalid-data
           move function trim (ws-user-id) to
             ws-numeric-reference
           move ws-numeric-reference to ws-user-id

           open i-o userfile

           if file-status = "35"
               move "1" to invalid-data
           end-if
           move ws-user-id to user-id of userrec

           read userfile

               invalid key
                   move "1" to invalid-data.
           if not key-not-found
               move user-name of userrec to ws-user-name
               move "1" to ws-valid-user
           else
               move "User record does not exist." to ws-error
               move spaces to ws-user-id
               move spaces to ws-user-name
               move "0" to ws-valid-user
           end-if.

           close userfile.

       p521-search-property.
           move "0" to end-of-file
           open input propertyfile
           if file-status not equal "35"
               perform until eof
                   read propertyfile next record
                       at end
                           move "1" to end-of-file
                       not at end
                           if Property-Address1 = ws-property-name
                               move property-id of Property-Record
                                 to ws-property-id
                           end-if
               end-perform
           end-if.

           close propertyfile.

           if ws-property-id not equal spaces
               perform p511-read-property
           else
               move "Property not found." to ws-error
           end-if.

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

       p523-search-user.
           move "0" to end-of-file
           open input userfile
           if file-status not equal "35"
               perform until eof
                   read userfile next record
                       at end
                           move "1" to end-of-file
                       not at end
                           if user-name = ws-user-name
                               move user-id of userrec to ws-user-id
                           end-if
               end-perform
           end-if.
           close userfile.
           if ws-user-id not equal spaces
               perform p513-read-user
           else
               move "User name not found." to ws-error
           end-if.

       end program 401Viewing.