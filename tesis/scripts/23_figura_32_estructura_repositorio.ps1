# Figura 32 - estructura resumida del repositorio (version final)
# Tesis de Daniela Salinas Castro
#
# Esta version muestra solo los archivos relevantes y las versiones
# definitivas de los scripts de reproducibilidad.

$ErrorActionPreference = "Stop"

chcp 65001 | Out-Null
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new($false)
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ProjectRoot

$OutputDirectory = Join-Path `
    (Split-Path -Parent $ProjectRoot) `
    "tesis\figuras"

New-Item `
    -ItemType Directory `
    -Path $OutputDirectory `
    -Force | Out-Null

$OutputFile = Join-Path `
    $OutputDirectory `
    "figura_32_estructura_repositorio_final.txt"

$Lines = New-Object System.Collections.Generic.List[string]

function Add-Line {
    param([string]$Text = "")

    $script:Lines.Add($Text)
    Write-Host $Text
}

function Add-FileIfExists {
    param(
        [string]$RelativePath,
        [string]$Indent = ""
    )

    $FullPath = Join-Path $ProjectRoot $RelativePath

    if (Test-Path $FullPath) {
        Add-Line ($Indent + "[F] " + $RelativePath)
    }
}

function Add-RelevantFiles {
    param(
        [string]$Directory,
        [string]$Pattern,
        [int]$Maximum,
        [string]$Indent = "    "
    )

    $FullDirectory = Join-Path $ProjectRoot $Directory

    if (-not (Test-Path $FullDirectory)) {
        return
    }

    $Matches = Get-ChildItem `
        -Path $FullDirectory `
        -File `
        -Recurse `
        -ErrorAction SilentlyContinue |
        Where-Object {
            $_.FullName -match $Pattern
        } |
        Sort-Object FullName |
        Select-Object -First $Maximum

    foreach ($Item in $Matches) {
        $RelativePath = $Item.FullName.Substring(
            $ProjectRoot.Length
        ).TrimStart("\")

        Add-Line ($Indent + "[F] " + $RelativePath)
    }
}

Clear-Host

Add-Line "ESTRUCTURA DE ARCHIVOS RELEVANTES DEL REPOSITORIO"
Add-Line "================================================"
Add-Line ""
Add-Line ("Repositorio: " + $ProjectRoot)

Add-Line ""
Add-Line "ARCHIVOS PRINCIPALES"
Add-Line "--------------------"

Add-FileIfExists "DESCRIPTION"
Add-FileIfExists "NAMESPACE"
Add-FileIfExists "README.md"

Add-Line ""
Add-Line "[D] R\"
Add-RelevantFiles `
    -Directory "R" `
    -Pattern "(?i)community|infomap" `
    -Maximum 5

Add-Line ""
Add-Line "[D] src\"
Add-RelevantFiles `
    -Directory "src" `
    -Pattern "(?i)infomap|community" `
    -Maximum 10

Add-Line ""
Add-Line "[D] tests\"
Add-RelevantFiles `
    -Directory "tests" `
    -Pattern "(?i)community|infomap" `
    -Maximum 5

Add-Line ""
Add-Line "EVIDENCIAS DE REPRODUCIBILIDAD"
Add-Line "------------------------------"

$FinalEvidenceFiles = @(
    "22_figura_31_validacion_powershell_v2.R",
    "22_figura_31_ejecucion_powershell_v3.ps1",
    "23_figura_32_estructura_repositorio_v2.ps1",
    "24_figura_33_rama_version_commit.ps1"
)

foreach ($File in $FinalEvidenceFiles) {
    Add-FileIfExists $File
}

Add-Line ""
Add-Line "RESULTADO: ESTRUCTURA FINAL GENERADA CORRECTAMENTE"

$Lines |
    Out-File `
        -FilePath $OutputFile `
        -Encoding utf8 `
        -Width 300

Write-Host ""
Write-Host "Salida guardada en:" -ForegroundColor Green
Write-Host $OutputFile
