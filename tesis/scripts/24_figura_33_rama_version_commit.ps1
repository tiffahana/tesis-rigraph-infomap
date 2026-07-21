# Figura 33 - rama, version y commit del repositorio
# Tesis de Daniela Salinas Castro
#
# Ejecutar desde PowerShell en la raiz de rigraph-main.
# El script registra la rama activa, la version del paquete,
# el repositorio remoto y el commit actual.

$ErrorActionPreference = "Stop"

chcp 65001 | Out-Null
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new($false)
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ProjectRoot

$GitCommand = Get-Command git.exe -ErrorAction SilentlyContinue

if ($null -eq $GitCommand) {
    throw "No se encontro git.exe en el sistema."
}

if (-not (Test-Path (Join-Path $ProjectRoot ".git"))) {
    throw "La carpeta seleccionada no corresponde a un repositorio Git."
}

$DescriptionFile = Join-Path $ProjectRoot "DESCRIPTION"

if (-not (Test-Path $DescriptionFile)) {
    throw "No se encontro el archivo DESCRIPTION."
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
    "figura_33_rama_version_commit.txt"

$PackageVersionLine = Get-Content $DescriptionFile |
    Where-Object {
        $_ -match "^Version:"
    } |
    Select-Object -First 1

$PackageVersion = (
    $PackageVersionLine -replace "^Version:\s*", ""
).Trim()

$Branch = (
    git rev-parse --abbrev-ref HEAD
).Trim()

$CommitShort = (
    git rev-parse --short HEAD
).Trim()

$CommitFull = (
    git rev-parse HEAD
).Trim()

$CommitDate = (
    git log -1 --format="%ad" --date=iso-strict
).Trim()

$CommitAuthor = (
    git log -1 --format="%an"
).Trim()

$CommitMessage = (
    git log -1 --format="%s"
).Trim()

$Remote = (
    git remote get-url origin 2>$null
)

if ([string]::IsNullOrWhiteSpace($Remote)) {
    $Remote = "No configurado"
}
else {
    $Remote = $Remote.Trim()
}

# Ignora archivos no rastreados, como scripts auxiliares y figuras.
$TrackedChanges = @(
    git status --short --untracked-files=no
)

if ($TrackedChanges.Count -eq 0) {
    $RepositoryState = "SIN CAMBIOS RASTREADOS PENDIENTES"
}
else {
    $RepositoryState = "CON CAMBIOS RASTREADOS PENDIENTES"
}

$Lines = @(
    "IDENTIFICACION DE LA VERSION DEL PROTOTIPO",
    "==========================================",
    "",
    "Repositorio local:",
    $ProjectRoot,
    "",
    "Repositorio remoto:",
    $Remote,
    "",
    "Rama activa:",
    $Branch,
    "",
    "Version del paquete:",
    $PackageVersion,
    "",
    "Commit actual:",
    "  Abreviado: $CommitShort",
    "  Completo:  $CommitFull",
    "  Fecha:     $CommitDate",
    "  Autor:     $CommitAuthor",
    "  Mensaje:   $CommitMessage",
    "",
    "Estado del repositorio:",
    $RepositoryState,
    "",
    "RESULTADO: VERSION DEL PROTOTIPO IDENTIFICADA CORRECTAMENTE"
)

Clear-Host

foreach ($Line in $Lines) {
    Write-Host $Line
}

$Lines |
    Out-File `
        -FilePath $OutputFile `
        -Encoding utf8 `
        -Width 300

Write-Host ""
Write-Host "Salida guardada en:" -ForegroundColor Green
Write-Host $OutputFile
