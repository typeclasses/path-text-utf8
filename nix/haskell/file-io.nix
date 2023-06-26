{ mkDerivation, base, bytestring, filepath, lib, unix }:
mkDerivation {
  pname = "file-io";
  version = "0.1.0.1";
  sha256 = "84142deac605e755541479ecef0c8177f3295afe62d637f6a1b5feb11b13b9cf";
  revision = "1";
  editedCabalFile = "0kfisk0vrjviw194rg2ildzr0qlg45wk4cwa4s3qpl3hp4zag1lj";
  libraryHaskellDepends = [ base bytestring filepath unix ];
  homepage = "https://github.com/hasufell/file-io";
  description = "Basic file IO operations via 'OsPath'";
  license = lib.licenses.bsd3;
}
