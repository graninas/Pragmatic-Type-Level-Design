{-# LANGUAGE DataKinds     #-}

module D2p2 where

import GUI.Components

type FirstName = TextBox 10 "Enter your first name"
type LastName  = TextBox 20 "Enter your last name"


type NameForm = RowLayout
  '[ FirstName
   , LastName
   ]

type PasswordForm = ColumnLayout
  '[ NameForm
   , TextBox 24 "Enter your password"
   ]

-- Resulting qml:

-- import QtQuick 2.7
-- import QtQuick.Controls 2.3
-- import QtQuick.Layouts 1

-- ColumnLayout {
--     RowLayout {
--         Text {
--           text: "Enter your first name"
--           font.pointSize: 10
--         }
--         Text {
--           text: "Enter your last name"
--           font.pointSize: 20
--         }
--     }
--     Text {
--         text: "Enter your password"
--         font.pointSize: 24
--     }
-- }




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
