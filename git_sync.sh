#!/bin/bash
# Sync complet: add -> commit si nécessaire -> pull --rebase -> push

set -euo pipefail

REPO_DIR="/Users/kimmoun/Library/CloudStorage/Dropbox/Travail/Année 2024-2025/ADRECMO"
BRANCH="$(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || echo main)"

cd "$REPO_DIR"

echo "==> Repo: $REPO_DIR"
echo "==> Branche: $BRANCH"

# 1) Ajoute les changements (respecte .gitignore)
git add -A

# 2) Commit seulement s’il y a des fichiers en staging
if git diff --cached --quiet; then
  echo "==> Rien à committer."
else
  TS="$(date '+%Y-%m-%d %H:%M:%S')"
  git commit -m "Auto sync: $TS"
fi

# 3) Récupérer du remote (rebase pour historique propre)
git fetch --all --prune
if ! git pull --rebase origin "$BRANCH"; then
  echo "⚠️  Conflits pendant le rebase. Résous-les puis:"
  echo "   git add <fichiers> && git rebase --continue"
  exit 1
fi

# 4) Pousser vers GitHub
git push origin "$BRA✅ Sync terminé."
