# Compile-Time Cache Benchmark Script for izumi-reflect
# Measures compile-time improvement from caching

param(
    [int]$Iterations = 3
)

Write-Host "=============================================="
Write-Host "  izumi-reflect Compile-Time Cache Benchmark"
Write-Host "=============================================="
Write-Host ""

Write-Host "Cleaning build artifacts..."
sbt clean 2>&1 | Out-Null

function Measure-Compile {
    param([string]$ScalaVersion, [bool]$WithCache)
    
    sbt "++ $ScalaVersion" "izumi-reflectJVM/clean" 2>&1 | Out-Null
    
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    
    if ($WithCache) {
        sbt "++ $ScalaVersion" "izumi-reflectJVM/Test/compile" 2>&1 | Out-Null
    } else {
        # Use environment variable - sbt passes it to forked compiler JVM
        $env:SBT_OPTS = "-Dizumi.reflect.rtti.cache.compile=false"
        sbt "++ $ScalaVersion" "izumi-reflectJVM/Test/compile" 2>&1 | Out-Null
        $env:SBT_OPTS = ""
    }
    
    $sw.Stop()
    return $sw.Elapsed.TotalSeconds
}

function Benchmark {
    param([string]$Version, [string]$Label)
    
    Write-Host "`nBenchmarking $Label..."
    
    $with = @(); $without = @()
    
    for ($i = 1; $i -le $Iterations; $i++) {
        Write-Host "  With cache $i/$Iterations..."
        $t = Measure-Compile -ScalaVersion $Version -WithCache $true
        $with += $t
        Write-Host "    $([math]::Round($t,2))s"
    }
    
    for ($i = 1; $i -le $Iterations; $i++) {
        Write-Host "  Without cache $i/$Iterations..."
        $t = Measure-Compile -ScalaVersion $Version -WithCache $false
        $without += $t
        Write-Host "    $([math]::Round($t,2))s"
    }
    
    $avgWith = ($with | Measure-Object -Average).Average
    $avgWithout = ($without | Measure-Object -Average).Average
    $imp = (($avgWithout - $avgWith) / $avgWithout) * 100
    
    return @{ With = $avgWith; Without = $avgWithout; Improvement = $imp }
}

$s2 = Benchmark -Version "2.13.14" -Label "Scala 2"
$s3 = Benchmark -Version "3.3.6" -Label "Scala 3"

Write-Host "`n=============================================="
Write-Host "                 RESULTS"
Write-Host "==============================================`n"

Write-Host "Scala 2 cache: $([math]::Round($s2.With, 2))s"
Write-Host "Scala 2 without cache: $([math]::Round($s2.Without, 2))s"
$color = if ($s2.Improvement -gt 0) { "Green" } else { "Red" }
Write-Host "Improved: $([math]::Round($s2.Improvement, 1))%" -ForegroundColor $color

Write-Host ""

Write-Host "Scala 3 cache: $([math]::Round($s3.With, 2))s"
Write-Host "Scala 3 without cache: $([math]::Round($s3.Without, 2))s"
$color = if ($s3.Improvement -gt 0) { "Green" } else { "Red" }
Write-Host "Improved: $([math]::Round($s3.Improvement, 1))%" -ForegroundColor $color
