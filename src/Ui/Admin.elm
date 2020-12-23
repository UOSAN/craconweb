module Ui.Admin exposing
    ( adminPage
    , editUserPage
    , mesPage
    , registerPage
    )

import Dropdown exposing (Options)
import Entity
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Model exposing (AdminModel, Model, Msg(..))
import Routing as R
import Ui.Parts as Parts


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model.glitching
        [ adminTop model.user
        , usersTable model
        ]


basicAdminPage : Maybe String -> List (Html Msg) -> Html Msg
basicAdminPage glitching children =
    section [ class "section gray" ]
        [ div
            [ class "container" ]
            ([ Parts.notification glitching "is-warning" ] ++ children)
        ]


mesPage : Model -> Html Msg
mesPage model =
    basicAdminPage model.glitching
        [ backButton
        , h1 [ class "title" ] [ text "Motivational Enhancement Statements" ]
        , p [ class "subtitle" ] [ text "Read and approve statements" ]
        , hr [] []
        , mesTable model.adminModel
        ]


publishButton : String -> Html Msg
publishButton id =
    a
        [ class "button is-success is-outlined"
        , PublishMes id |> onClick
        ]
        [ text "Publish!" ]


mesTable : AdminModel -> Html Msg
mesTable am =
    case am.mesAnswers of
        Just mesAnswers ->
            mesAnswers
                |> List.map (\ms -> tr [] [ td [] [ text ms.essay ], td [] [ publishButton ms.id ] ])
                |> mesTableHelper

        Nothing ->
            Parts.middleBlock [ p [] [ text "No statements yet! Check back later." ] ]


mesTableHelper : List (Html Msg) -> Html Msg
mesTableHelper rows =
    table [ class "table is-bordered is-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Essay" ], th [] [ text "Actions" ] ]
            ]
        , tbody [] rows
        ]


adminTop : Maybe Entity.User -> Html Msg
adminTop user =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ h1 [ class "title" ] [ text "Admin" ]
            , div [ class "field" ]
                [ a
                    [ class "button"
                    , href R.registerPath
                    , R.onLinkClick <| UpdateLocation R.registerPath
                    ]
                    [ span [ class "icon is-small" ]
                        [ i [ class "fa fa-user-o" ]
                            []
                        ]
                    , span []
                        [ text "Register User" ]
                    ]
                , text " "
                , a
                    [ class "button"
                    , href R.mesPath
                    , R.onLinkClick <| UpdateLocation R.mesPath
                    ]
                    [ span [ class "icon is-small" ]
                        [ i [ class "fa fa-comments-o" ]
                            []
                        ]
                    , span []
                        [ text "Approve MES" ]
                    ]
                , text " "
                , a
                    [ class "button"
                    , href R.homePath
                    , R.onLinkClick <| UpdateLocation R.homePath
                    ]
                    [ span [ class "icon is-small" ]
                        [ i [ class "fa fa-gamepad" ]
                            []
                        ]
                    , span []
                        [ text "Participant View" ]
                    ]
                , text " "
                , a [ class "button", onClick Logout ]
                    [ span [ class "icon is-small" ]
                        [ i [ class "fa fa-sign-out" ]
                            []
                        ]
                    , span []
                        [ text "Logout" ]
                    ]
                ]
            ]
        ]


registerPage : Model -> Html Msg
registerPage model =
    basicAdminPage model.glitching
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1
                    [ class "title" ]
                    [ text <| "Register a new user" ]
                , registerUserForm model
                ]
            ]
        ]


usersTable : Model -> Html Msg
usersTable model =
    table [ class "table is-bordered is-striped is-narrow" ]
        [ thead []
            [ tr []
                [ th [] [ text "Subject" ]
                , th [] [ text "First Name" ]
                , th [] [ text "Last Name" ]
                , th [] [ text "Username" ]
                , th [] [ text "fMRI" ]
                , th [] [ text "Email" ]
                , th [] [ text "Group" ]
                , th [] [ text "Actions" ]
                ]
            ]
        , tbody [] (userRows ( Maybe.withDefault "" model.groupIdExp, Maybe.withDefault "" model.groupIdCon ) model.users model.request)
        ]


userRows : ( String, String ) -> List Entity.User -> Maybe String -> List (Html Msg)
userRows ( expId, conId ) users request =
    let
        groupToString id =
            if id == expId then
                "Experimental"

            else if id == conId then
                "Control"

            else
                "?"

        row user =
            tr []
                [ td [] [ text user.subject ]
                , td [] [ text user.firstName ]
                , td [] [ text user.lastName ]
                , td [] [ text user.username ]
                , td []
                    [ a [ class "button is-small", onClick (UpdateLocation ("/fmri/" ++ user.id)) ]
                        [ span [ class "icon is-small" ]
                            [ i [ class "fa fa-gamepad" ]
                                []
                            ]
                        , span []
                            [ text "Start" ]
                        ]
                    ]
                , td [] [ text user.email ]
                , td [] [ text <| groupToString user.groupId ]
                , td []
                    [ a
                        [ class "button is-small"
                        , onClick (FillTmpUserEdit user.id)
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fa fa-wrench" ]
                                []
                            ]
                        , span
                            []
                            [ text "Edit" ]
                        ]
                    ]
                ]
    in
    users
        |> List.map row


iconButton : String -> String -> String -> String -> Html Msg
iconButton text_ path icon mods =
    a
        [ class <| "button " ++ mods
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ span
            [ class <| "icon " ++ mods ]
            [ i
                [ class <| "fa " ++ icon ]
                []
            ]
        , span
            []
            [ text text_
            ]
        ]


bButton : String -> String -> String -> Html Msg
bButton title path mods =
    a
        [ class ("button " ++ mods)
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


editUser404 : Html Msg
editUser404 =
    basicAdminPage Nothing
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1 [ class "title" ] [ text "User not found" ] ]
            ]
        ]


divColumns : List (Html Msg) -> Html Msg
divColumns children =
    div [ class "columns" ] children


editUserPage : Model -> String -> Html Msg
editUserPage model userid =
    case model.adminModel.tmpUserEdit of
        Just user ->
            editUser
                model.groupIdExp
                model.groupIdCon
                model.informing
                model.tasksrv
                user
                model.ugimgsets

        Nothing ->
            editUser404


editUser :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> String
    -> Model.UserEdit
    -> Maybe (List Entity.Ugimgset)
    -> Html Msg
editUser exp con informing tasksrv user ugimgsets =
    basicAdminPage Nothing
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ Parts.notification informing "is-info is-small"
                , backButton
                , hr [] []
                , userForm user
                    exp
                    con
                    (case informing of
                        Nothing ->
                            False

                        Just _ ->
                            True
                    )
                ]
            ]
        , divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ uploadCsvForm tasksrv user.id
                , br [] []
                , text "Download the control group image set "
                , a [ href "https://file.cravecontrol.org/repo/controlimgs.csv" ] [ text "csv file" ]
                , text "."
                ]
            ]
        ]


userForm :
    Model.UserEdit
    -> Maybe String
    -> Maybe String
    -> Bool
    -> Html Msg
userForm user exp con saving =
    Html.form []
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "First Name"
                    , input
                        [ class "input"
                        , type_ "text"
                        , value user.firstName
                        , onInput (SetTmpUserEdit "firstName")
                        ]
                        []
                    ]
                ]
            , div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "Last Name"
                    , input
                        [ class "input"
                        , type_ "text"
                        , value user.lastName
                        , onInput (SetTmpUserEdit "lastName")
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "Username"
                    , input
                        [ class "input"
                        , type_ "text"
                        , value user.username
                        , onInput (SetTmpUserEdit "username")
                        ]
                        []
                    ]
                ]
            , div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "Email"
                    , input
                        [ class "input"
                        , type_ "text"
                        , value user.email
                        , onInput (SetTmpUserEdit "email")
                        ]
                        []
                    ]
                ]
            , div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "Password"
                    , input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Hidden"
                        , onInput (SetTmpUserEdit "password")
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ label_ "MES Opt-In"
                , checkboxInput ToggleTmpUserEditMesOptin user.mesOptin
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ p [ class "control" ]
                    [ label_ "Group"
                    , span
                        [ class "select" ]
                        [ groupsDropDown exp con user.groupId ]
                    ]
                ]
            ]
        , a
            [ class "button is-primary"
            , onClick TryPutUser
            ]
            [ text "Save User" ]
        ]


uploadCsvForm : String -> String -> Html Msg
uploadCsvForm tasksrv userId =
    Html.form
        [ enctype "multipart/form-data"
        , name "csvfile"
        , action <| tasksrv ++ "/upload/ugimgset"
        , method "POST"
        , id "csvForm"
        , class "box"
        ]
        [ h4
            [ class "title is-4" ]
            [ text "Upload image set"
            ]
        , input
            [ type_ "file"
            , id "csvFilInput"
            , accept ".csv"
            , name "upload"
            ]
            []
        , input
            [ type_ "hidden"
            , id "csvFilInput"
            , name "userid"
            , value userId
            ]
            []
        , hr [] []
        , editButtons
        ]


editButtons : Html Msg
editButtons =
    div
        [ class "field is-grouped" ]
        [ a
            [ class "button is-primary", onClick TryCsvUpload ]
            [ span
                [ class "icon" ]
                [ i
                    [ class "fa fa-file-text-o" ]
                    []
                ]
            , span
                []
                [ text "Upload CSV"
                ]
            ]
        , text " "
        ]


checkboxInput : Msg -> Bool -> Html Msg
checkboxInput msg chked =
    p [ class "control" ]
        [ input
            [ class "checkbox"
            , type_ "checkbox"
            , checked chked
            , onClick msg
            ]
            []
        ]


textInput : String -> String -> Html Msg
textInput field_ placeholder_ =
    p [ class "control" ]
        [ input
            [ class "input"
            , placeholder placeholder_
            , type_ "text"
            , onInput <| SetRegistration field_
            ]
            []
        ]


emailInput : String -> String -> Html Msg
emailInput field_ placeholder_ =
    p [ class "control" ]
        [ input
            [ class "input"
            , placeholder placeholder_
            , type_ "email"
            , onInput <| SetRegistration field_
            ]
            []
        ]


label_ : String -> Html Msg
label_ title =
    label [ class "label" ]
        [ text title ]


firstLastReg : Html Msg
firstLastReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label_ "First Name *"
            , textInput "firstName" ""
            ]
        , div [ class "column" ]
            [ label_ "Last Name"
            , textInput "lastName" ""
            ]
        ]


userEmailPassReg : Html Msg
userEmailPassReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label_ "Username *"
            , textInput "username" "joey123"
            ]
        , div [ class "column" ]
            [ label_ "Email"
            , emailInput "email" "joe@example.com"
            ]
        , div [ class "column" ]
            [ label_ "Password *"
            , textInput "password" "longfancyphrase"
            ]
        ]


checkboxesReg : Model -> Html Msg
checkboxesReg model =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label_ "MES Opt-In"
            , checkboxInput ToggleRegistrationMesOptin model.adminModel.tmpUserRecord.mesOptin
            ]
        ]


dropdownOptions : Maybe String -> Maybe String -> Options Msg
dropdownOptions groupIdExp groupIdCon =
    let
        defaultOptions =
            Dropdown.defaultOptions GroupChanged

        items_ =
            case ( groupIdExp, groupIdCon ) of
                ( Just e, Just c ) ->
                    [ { value = e, text = "Experimental", enabled = True }
                    , { value = c, text = "Control", enabled = True }
                    ]

                _ ->
                    []
    in
    { defaultOptions
        | items = items_
        , emptyItem = Just { value = "0", text = "Select a group...", enabled = True }
    }


groupsDropDown : Maybe String -> Maybe String -> String -> Html Msg
groupsDropDown exp con groupId =
    Dropdown.dropdown
        (dropdownOptions exp con)
        []
        (Just groupId)


registerUserForm : Model -> Html Msg
registerUserForm model =
    Html.form
        [ onSubmit TryRegisterUser ]
        [ firstLastReg
        , userEmailPassReg
        , checkboxesReg model
        , div [ class "field" ]
            [ label_ "Group *"
            , p
                [ class "control" ]
                [ span
                    [ class "select" ]
                    [ groupsDropDown model.groupIdExp model.groupIdCon model.adminModel.tmpUserRecord.groupId
                    ]
                ]
            ]
        , hr []
            []
        , regButtons model.loading
        ]


regButtons : Maybe String -> Html Msg
regButtons loading =
    let
        class_ =
            case loading of
                Just l ->
                    "button is-primary is-loading"

                Nothing ->
                    "button is-primary"
    in
    div
        [ class "field is-grouped" ]
        [ button
            [ class class_ ]
            [ text "Submit" ]
        , button
            ([ class "button is-link" ] ++ Parts.linkAttrs R.adminPath)
            [ text "Cancel" ]
        ]


primaryButton : String -> String -> Html Msg
primaryButton title path =
    a
        [ class "button is-primary"
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


backButton : Html Msg
backButton =
    button
        ([ class "button is-link" ] ++ Parts.linkAttrs R.adminPath)
        [ text "Go back" ]
