$Script = Invoke-WebRequest 'https://raw.githubusercontent.com/ndmitchell/neil/master/misc/appveyor.ps1'
$ScriptBlock = [Scriptblock]::Create($Script.Content)
Invoke-Command -ScriptBlock $ScriptBlock -ArgumentList (@('hlint') + $args)
