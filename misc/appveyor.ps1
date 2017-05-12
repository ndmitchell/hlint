$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/neil/master/misc/appveyor.ps1'
Invoke-Command ([Scriptblock]::Create($Script.Content)) -ArgumentList (@('hlint') + $args)
