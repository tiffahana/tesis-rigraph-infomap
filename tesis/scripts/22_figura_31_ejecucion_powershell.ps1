# Figura 31 - ejecucion de pruebas mediante PowerShell
# Version compatible con Windows PowerShell 5.1.
#
# Correccion:
# Tee-Object de Windows PowerShell 5.1 no admite el parametro -Encoding.
# Por eso la salida se captura primero y luego se guarda con Out-File.

$ErrorActionPreference = "Stop"

# Configurar la consola en UTF-8.
chcp 65001 | Out-Null
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new($false)
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ProjectRoot

$Rscript = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"

if (-not (Test-Path $Rscript)) {
    $RscriptCommand = Get-Command Rscript.exe -ErrorAction SilentlyContinue

    if ($null -eq $RscriptCommand) {
        throw "No se encontro Rscript.exe."
    }

    $Rscript = $RscriptCommand.Source
}

$ValidationScript = Join-Path `
    $ProjectRoot `
    "22_figura_31_validacion_powershell_v2.R"

if (-not (Test-Path $ValidationScript)) {
    throw "No se encontro el script de validacion: $ValidationScript"
}

$OutputDirectory = Join-Path `
    (Split-Path -Parent $ProjectRoot) `
    "tesis\figuras"

New-Item `
    -ItemType Directory `
    -Path $OutputDirectory `
    -Force | Out-Null

$OutputFile = Join-Path `
    $OutputDirectory `
    "figura_31_salida_pruebas_powershell.txt"

Clear-Host

Write-Host "EJECUCION DE PRUEBAS DEL PROTOTIPO" -ForegroundColor Cyan
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Directorio del proyecto:"
Write-Host $ProjectRoot
Write-Host ""
Write-Host "Comando ejecutado:"
Write-Host "& `"$Rscript`" `".\22_figura_31_validacion_powershell_v2.R`""
Write-Host ""

# Ejecutar R y capturar toda la salida.
$TestOutput = & $Rscript $ValidationScript 2>&1
$ExitCode = $LASTEXITCODE

# Mostrar la salida en la terminal.
$TestOutput | ForEach-Object {
    Write-Host $_
}

# Guardar la misma salida en UTF-8.
$TestOutput |
    Out-File `
        -FilePath $OutputFile `
        -Encoding utf8 `
        -Width 300

if ($ExitCode -ne 0) {
    throw "Rscript termino con el codigo de salida $ExitCode."
}

Write-Host ""
Write-Host "Salida guardada en:" -ForegroundColor Green
Write-Host $OutputFile
