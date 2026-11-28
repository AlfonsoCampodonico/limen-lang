#!/bin/sh
# The site operator (examples/site_operator.lmn) in a real cluster, with no
# kubectl proxy: it reads its service account and speaks TLS to the API
# server itself. Creates a kind cluster, builds the image
# (examples/k8s/Dockerfile) and loads it into the cluster, applies the CRD,
# the operator and a site, and waits for the site's objects and status.
# Needs Docker and kind. The cluster is deleted at the end.
#   sh test/integration/kind.sh
set -e
cd "$(dirname "$0")/../.."
cluster=limen-kind
image=limen-site-operator:dev
ctx=kind-$cluster
kc() { kubectl --context "$ctx" "$@"; }
cleanup() {
  kind delete cluster --name "$cluster" >/dev/null 2>&1 || true
}
trap cleanup EXIT
trap 'exit 1' INT TERM
fail() {
  echo "kind: $1"
  echo "--- the operator's pods"
  kc -n limen-system get pods -o wide 2>&1 || true
  echo "--- the operator's log"
  kc -n limen-system logs deployment/site-operator --tail=50 2>&1 || true
  echo "--- the site"
  kc get staticsite hello -o yaml 2>&1 || true
  exit 1
}
kind delete cluster --name "$cluster" >/dev/null 2>&1 || true
kind create cluster --name "$cluster" --wait 120s
docker build -f examples/k8s/Dockerfile -t "$image" .
kind load docker-image "$image" --name "$cluster"
kc apply -f examples/k8s/staticsite-crd.yaml
kc wait --for condition=established --timeout=60s crd/staticsites.limen.dev
kc apply -f examples/k8s/operator.yaml
kc apply -f examples/k8s/hello-site.yaml
# the ConfigMap, the Deployment, the Service and the status, within 120 s
i=0
until kc get configmap hello-html >/dev/null 2>&1 &&
  kc get deployment hello >/dev/null 2>&1 &&
  kc get service hello >/dev/null 2>&1 &&
  [ -n "$(kc get staticsite hello -o jsonpath='{.status.message}' 2>/dev/null)" ]; do
  i=$((i + 1))
  [ $i -lt 120 ] || fail "the operator did not reconcile hello within 120 s"
  sleep 1
done
if pgrep -f "kubectl.* proxy" >/dev/null 2>&1; then
  fail "a kubectl proxy is running"
fi
echo "--- the operator's log"
kc -n limen-system logs deployment/site-operator
echo "--- status: $(kc get staticsite hello -o jsonpath='{.status.message}')"
echo "kind: the operator reconciled hello without a proxy"
