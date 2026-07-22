# Genera un piloto factorial de redes LFR jerarquicas.
#
# Diseño:
#   mu1 = 0.05, 0.10, 0.20
#   mu2 = 0.20, 0.30, 0.40
#   semillas = 101, 202, 303
#
# Total: 3 x 3 x 3 = 27 redes.
#
# Ejecutar desde la raiz del repositorio:
#   powershell -ExecutionPolicy Bypass -File `
#     tesis\scripts\comparaciones\28_generar_piloto_lfr.ps1

$ErrorActionPreference = "Stop"

$repoDir = (Get-Location).Path

if (-not (Test-Path (Join-Path $repoDir "DESCRIPTION")) -or
    -not (Test-Path (Join-Path $repoDir "tesis"))) {
    throw "Ejecute este script desde la raiz del repositorio."
}

$generator = Join-Path `
    (Split-Path $repoDir -Parent) `
    "LFRbenchmarks\hierarchical\hbenchmark.exe"

if (-not (Test-Path $generator)) {
    throw "No se encontro el generador LFR en: $generator"
}

$outRoot = Join-Path `
    $repoDir `
    "tesis\experimentos\barrido_lfr\piloto"

New-Item -ItemType Directory -Path $outRoot -Force | Out-Null

# Parametros fijos.
$nodeCount = 1000
$averageDegree = 20
$maximumDegree = 50

# IMPORTANTE:
# PowerShell no distingue mayusculas de minusculas en los nombres de variables.
# Por eso no se deben usar pares como $minc/$minC ni $maxc/$maxC.
$microMin = 20
$microMax = 50
$macroMin = 100
$macroMax = 300

# Barrido factorial.
$mu1Values = @(0.05, 0.10, 0.20)
$mu2Values = @(0.20, 0.30, 0.40)
$seeds = @(101, 202, 303)

$ascii = [System.Text.Encoding]::ASCII
$invariant = [System.Globalization.CultureInfo]::InvariantCulture

function Format-Decimal {
    param([double]$Value)

    return [string]::Format(
        $invariant,
        "{0:0.00}",
        $Value
    )
}

$manifest = New-Object System.Collections.Generic.List[object]
$total = $mu1Values.Count * $mu2Values.Count * $seeds.Count
$actual = 0

foreach ($mu1 in $mu1Values) {
    foreach ($mu2 in $mu2Values) {
        if (($mu1 + $mu2) -gt 1) {
            throw "Configuracion invalida: mu1 + mu2 debe ser <= 1."
        }

        $mu1Text = Format-Decimal $mu1
        $mu2Text = Format-Decimal $mu2
        $sameMicro = 1 - $mu1 - $mu2
        $sameMicroText = Format-Decimal $sameMicro

        foreach ($seed in $seeds) {
            $actual++

            $configName = "mu1_${mu1Text}_mu2_${mu2Text}"
            $caseDir = Join-Path `
                (Join-Path $outRoot $configName) `
                "seed_$seed"

            New-Item `
                -ItemType Directory `
                -Path $caseDir `
                -Force |
                Out-Null

            $flagsPath = Join-Path $caseDir "flags.dat"
            $seedPath = Join-Path $caseDir "time_seed.dat"
            $metadataPath = Join-Path $caseDir "metadata.txt"
            $logPath = Join-Path $caseDir "generacion.log"

            $flags = @(
                "-N $nodeCount",
                "-k $averageDegree",
                "-maxk $maximumDegree",
                "-mu1 $mu1Text",
                "-minc $microMin",
                "-maxc $microMax",
                "-minC $macroMin",
                "-maxC $macroMax",
                "-mu2 $mu2Text"
            ) -join "`r`n"

            [System.IO.File]::WriteAllText(
                $flagsPath,
                $flags + "`r`n",
                $ascii
            )

            [System.IO.File]::WriteAllText(
                $seedPath,
                "$seed`r`n",
                $ascii
            )

            $metadata = @(
                "N=$nodeCount",
                "k=$averageDegree",
                "maxk=$maximumDegree",
                "minc=$microMin",
                "maxc=$microMax",
                "minC=$macroMin",
                "maxC=$macroMax",
                "mu1=$mu1Text",
                "mu2=$mu2Text",
                "fraccion_misma_micro=$sameMicroText",
                "semilla_inicial=$seed"
            ) -join "`r`n"

            [System.IO.File]::WriteAllText(
                $metadataPath,
                $metadata + "`r`n",
                $ascii
            )

            @(
                "network.dat",
                "community_first_level.dat",
                "community_second_level.dat"
            ) | ForEach-Object {
                $oldFile = Join-Path $caseDir $_

                if (Test-Path $oldFile) {
                    Remove-Item $oldFile -Force
                }
            }

            Write-Host (
                "[$actual/$total] mu1=$mu1Text " +
                "mu2=$mu2Text seed=$seed"
            ) -ForegroundColor Cyan

            $exitCode = -1
            $status = "ERROR"
            $errorMessage = ""

            Push-Location $caseDir

            try {
                # El ejecutable puede escribir mensajes informativos en stderr.
                # Se evita que esos mensajes detengan PowerShell antes de poder
                # leer el codigo de salida y revisar los archivos generados.
                $previousErrorAction = $ErrorActionPreference
                $ErrorActionPreference = "Continue"

                & $generator -f "flags.dat" 2>&1 |
                    Tee-Object -FilePath $logPath |
                    Out-Host

                $exitCode = $LASTEXITCODE
                $ErrorActionPreference = $previousErrorAction

                $required = @(
                    "network.dat",
                    "community_first_level.dat",
                    "community_second_level.dat"
                )

                $missing = $required |
                    Where-Object { -not (Test-Path $_) }

                if ($exitCode -eq 0 -and $missing.Count -eq 0) {
                    $status = "OK"
                }
                else {
                    $errorMessage = (
                        "Codigo de salida: $exitCode. " +
                        "Faltantes: " +
                        ($missing -join ", ")
                    )
                }
            }
            catch {
                $errorMessage = $_.Exception.Message
            }
            finally {
                $ErrorActionPreference = "Stop"
                Pop-Location
            }

            $relativeDir = $caseDir.Replace(
                $repoDir + "\",
                ""
            )

            $manifest.Add(
                [PSCustomObject]@{
                    configuracion = $configName
                    mu1 = $mu1Text
                    mu2 = $mu2Text
                    fraccion_misma_micro = $sameMicroText
                    semilla_inicial = $seed
                    estado = $status
                    codigo_salida = $exitCode
                    directorio = $relativeDir
                    error = $errorMessage
                }
            )

            $manifest |
                Export-Csv `
                    -Path (Join-Path $outRoot "manifesto_piloto.csv") `
                    -NoTypeInformation `
                    -Encoding UTF8
        }
    }
}

$okCount = (
    $manifest |
        Where-Object { $_.estado -eq "OK" }
).Count

$errorCount = $manifest.Count - $okCount

Write-Host ""
Write-Host "Piloto terminado." -ForegroundColor Green
Write-Host "Redes correctas: $okCount"
Write-Host "Redes con error: $errorCount"
Write-Host (
    "Manifesto: " +
    (Join-Path $outRoot "manifesto_piloto.csv")
)

if ($errorCount -gt 0) {
    exit 1
}
