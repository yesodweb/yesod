## yesod-form-multi

Support for creating forms in which the user can specify how many inputs to submit. Includes support for enforcing a minimum number of values.
Intended as an alternative to `Yesod.Form.MassInput`.

# Limitations
- If the user adds too many fields then there is currently no support for a "delete button" although fields submitted empty are considered to be deleted.