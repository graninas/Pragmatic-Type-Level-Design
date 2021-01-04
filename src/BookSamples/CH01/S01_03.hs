{-# LANGUAGE DataKinds     #-}

module BookSamples.CH01.S01_03 where

import GUI.Components (ColumnLayout, TextBox, TextField, TextFieldWithMode, PasswordEchoOnEdit)

type FirstName = TextBox 10 "firstName"
type LastName  = TextBox 20 "lastName"


type NameInput = ColumnLayout
  '[ FirstName
   , LastName
   ]


-- Sample of a QML component for entering login and password.

type LoginInput = ColumnLayout
  '[ TextBox 24 "Please, enter your login and password below:"
   , TextField "login" "Username"
   , TextFieldWithMode "password" "Password" PasswordEchoOnEdit
   ]

-- Will produce the following QML definition:
--
-- ColumnLayout {
--     Text {
--       text: "Please, enter your login and password below:"
--       font.pointSize: 24
--     }
--     TextField {
--         id: login
--         placeholderText: "Username"
--     }
--     TextField {
--         id: password
--         placeholderText: "Password"
--         echoMode: TextInput.PasswordEchoOnEdit
--     }
-- }
