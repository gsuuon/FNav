dotnet pack

try {
  dotnet tool update --global --add-source ./nupkg FNav
} catch {
  dotnet tool install --global --add-source ./nupkg FNav
}
