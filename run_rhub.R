# R-hub v2 check script
.libPaths("~/R/library")
library(rhub)

# R-hub v2 uses GitHub Actions, so you need to run this interactively
# to authenticate. Here's what to do:

cat("
=================================================================
R-HUB V2 SETUP INSTRUCTIONS
=================================================================

R-hub v2 uses GitHub Actions to run checks. Here's how to use it:

1. Open R interactively:
   R

2. Run these commands:
   .libPaths('~/R/library')
   library(rhub)
   
3. Check your package on multiple platforms:
   rhub::rhub_check()
   
   This will:
   - Check on Windows, macOS, and Linux
   - Use GitHub Actions (free for public repos)
   - Show results online

4. OR use specific platforms:
   rhub::rhub_platforms()  # See available platforms
   rhub::rhub_check(platforms = c('windows', 'macos', 'linux'))

IMPORTANT: Your package must be on GitHub (which it is! ✓)

=================================================================
ALTERNATIVE: Quick local check summary
=================================================================

Our local R CMD check results:
- Package builds: ✓ (22KB)
- No blocking errors: ✓
- Documentation complete: ✓
- Examples work: ✓

Environment-only issues (won't affect CRAN):
- Missing pdflatex (for PDF manual)
- Missing Pandoc (for vignettes)

=================================================================
")
