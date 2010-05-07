> module Yahc.Error where

> errEmptyDer, errMissingCmd, errNoSuchRule :: String

> errEmptyDer = "No has empezado ninguna demostración " ++
>               "para hacerlo usa el comando \"start\"."

> errMissingCmd = "No has ingresado un comando válido."

> errNoSuchRule = "No existe esa regla. Puedes ver las reglas existentes "++
>                 "haciendo \"show rules\"."

> errNoStep = "No se puede agregar una regla porque no hay nada demostrado."

> errBadStep = "No es una aplicación válida de la regla: "

> errFewParams = "No has ingresado suficientes parámetros. A continuación, "++
>                "cómo usar este comando."