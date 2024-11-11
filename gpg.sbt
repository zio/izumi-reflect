useGpg := false
pgpPassphrase := Some("password".toCharArray)
pgpSecretRing := file(".secrets/gnupg/secring.gpg")
pgpPublicRing := file(".secrets/gnupg/pubring.gpg")