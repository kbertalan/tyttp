module Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Maybe
import Node.HTTP2.Client
import Node.HTTP2.Server
import TyTTP.Adapter.Node.HTTP2
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

key : String
key = """
      -----BEGIN RSA PRIVATE KEY-----
      MIIEpAIBAAKCAQEAuDFIjxp80ogod4AEaBKi7X9k1vHiPzrzcXr0FE6ZUP+YNill
      8a6K5dRoLU6wDUfq5VPAMC3vrzuMDF9g2MVkBoNCBawXLd+64HO+P4fiNTe/nnww
      S764HWuLuKYFtNpo3LOke9/B2YHh1jVAVRl88D8xdHBRmqWnglW/06J0EbmSMyRM
      RUgdOXL7SSQkXW1OTsZIDtpyAQdwb5qlISJHrzGK3df9a1BOxC0YkVCtgWpkZQJ8
      2wqrS1sYKJdmzZrPVnyU7nhCqcftNcrxMUqrTDNk5e3mD3Z9pEmyliSGaR7UiODc
      cBu3lv+BvVEQV9IiqrIYzUvAtNkbeZHCeLPOgQIDAQABAoIBAQCyjIuitPF1ACoC
      FSWwYQhxIwH0XsuLsbCVO7PpD3wNiYbaUe0Rh7n7KpjF+rYsWZ+5KCNwUMc8XYPX
      5okx+L6KlahlaTma7Xrw4yHz4dwE7IKitg33DuzZEAyENOPpOaNW9gknT3NRTldG
      V56CH8/977QYBF0GhThI3qiyFZNgLZbglHpBIiUkPyxf7xC17HHXUs5jbYTjb7LF
      WU7b9LVlduTXnfYPZJrmQuiljElINH0lKpUFF0Y2Z+all05G8zXCIsH1lcDmV5P1
      NVfpAp9IGTy46bZcM5W+8UH/9I8NCMRYaQo9FxFdZrJ4hkmpIDyO99O4HkNybMFJ
      VM/wmbvBAoGBAOGAZOFtEMWcrCkMHpL1XzjMXIjmYcdGCoOPDYo88TfDvQiKsCQH
      ZHPrRuFV/uinJQgy6Fz8n6NJ4F/qlhbvjwaehwcFXquHeh0jUEez+stOk/GQPHx9
      7Dz3HUk5tTA5HaRRrFIZhd3HvdErP0zvStCJIq4NRJ3iapXIwIBEvr25AoGBANEa
      ooV4WFPwl5t3g7OhYFFyjLu8aJApv6+PzPQ//COpC5NbKttKd6Jnh5uc1VRwei74
      5vWOAlf3r9WZrQtaqDqCubGD5GzXpgyHrCNAjwv0lAXsyYhlToZ53SYCMEKsbM6M
      VIVp2Is3ovM0Pm1hrbh+4J+2S9wJz3se3OYxMbsJAoGAO/MVXrTPI6oOPu9g5XLk
      OMZjx2VwEzk0rFsIn3qqKkgYZj/wqfNpKkQfWOKy3QkqmC5ohegHTLLkOruz4S8Y
      2AqnHR+5VmKy/TIZMqbPguNGA49z9SjI+EA+AeycldBpZ/jqlf0BEL/8X7JepsZr
      HluG47zVRCAgc+el/bPI/ckCgYAndi9VoPCKZs01uchRDhNvS37jrxrMmhRivXCg
      Z7Ldx/k3Re1AlT0emQsugtp/pPpqAu8TSEmI8GVumiHeEq3M3P7OUWW6ZLghhTkn
      LffQ+cc1lwoxg2zeVAE4OxGAg6nFERaiVudW3pkR7LobSmPOfaNGgNCwJenSAnEN
      wBrygQKBgQCz7vNymJSIU1aU439fRnRNg2PvEZ7f2RhIMxIUhwHTmrCWw5xGGxr+
      6nc55n6+NHWk7K3JPDFLvk78ywHwBJWWxGDx2bmylwYaHSoN2TWt/S/fYtm4W4rm
      ZfIrbaEvFAY732U/f9wyml4Pv8lunyYq4gfwYtnOWRPu+CNvpVTxaw==
      -----END RSA PRIVATE KEY-----
      """

cert : String
cert = """
      -----BEGIN CERTIFICATE-----
      MIIDETCCAfkCFCnZR6cI1+chRF6r47aWtqim/K7IMA0GCSqGSIb3DQEBCwUAMEUx
      CzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl
      cm5ldCBXaWRnaXRzIFB0eSBMdGQwHhcNMjIwNjAyMTkyOTEyWhcNMjMwNjAyMTky
      OTEyWjBFMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UE
      CgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIIBIjANBgkqhkiG9w0BAQEFAAOC
      AQ8AMIIBCgKCAQEAuDFIjxp80ogod4AEaBKi7X9k1vHiPzrzcXr0FE6ZUP+YNill
      8a6K5dRoLU6wDUfq5VPAMC3vrzuMDF9g2MVkBoNCBawXLd+64HO+P4fiNTe/nnww
      S764HWuLuKYFtNpo3LOke9/B2YHh1jVAVRl88D8xdHBRmqWnglW/06J0EbmSMyRM
      RUgdOXL7SSQkXW1OTsZIDtpyAQdwb5qlISJHrzGK3df9a1BOxC0YkVCtgWpkZQJ8
      2wqrS1sYKJdmzZrPVnyU7nhCqcftNcrxMUqrTDNk5e3mD3Z9pEmyliSGaR7UiODc
      cBu3lv+BvVEQV9IiqrIYzUvAtNkbeZHCeLPOgQIDAQABMA0GCSqGSIb3DQEBCwUA
      A4IBAQAbkGEnyBs7u0j2K/Tf9u6bnVcjZRQyuZx9EpY6PbfolNkI97h6ymS8XORY
      fEuEDzotkurlOLudI78zApn5T3lbRM1uDlIc/2cGL+cypHQILH4EAtLz6rZm56OU
      tKghhph9fXsEBITHBYrZvbrjtmyaWHFtviTKKE5q/yAmUm/6Usj/ToVM6JVyAoGn
      isWatlB9Qm/TZv6ICATfXF+RWAEJZVZObhrZA2+WSCXd65HwPjf/Qr991h1Wy78D
      F+goS3KhK87p/MA3RQaoWr+/Nw98pESbOIrgmqnrPdUxhO45jZLlTgg+39WN0t79
      WsaxeBOETSTHbCsshp+/B+BzRlvK
      -----END CERTIFICATE-----
      """

main : IO ()
main = do
  http2 <- HTTP2.require
  let secureOptions = MkSecureOptions key cert
  ignore $ HTTP2.Secure.secureListen' secureOptions {e = String, pushIO = IO} $ \push =>
      routes' (text "Resource could not be found" >=> status NOT_FOUND)
        [ get $ path "/query" $ \step =>
            text step.request.url.search step >>= status OK
        , get $ path "/parsed" $ Simple.search $ \step =>
            text (show step.request.url.search) step >>= status OK
        , get $ path "/request" :> \ctx => do
            putStrLn "Calling https"
            (headers, stream) <- MkPromise $ \cb => do
              session <- http2.connect "https://localhost:3000" $ {
                  rejectUnauthorized := False
                } Connect.defaultOptions
              stream <- session.get "/parsed?q=from-request" =<< empty
              stream.onResponse $ \headers => cb.onSucceded (headers, stream)
              onError stream $ \error => cb.onFailed error

            pure $
              { response.status := OK
              , response.headers := [("Content-Type", "text/plain")]
              , response.body := MkPublisher $ \s => do
                  onData stream s.onNext
                  onEnd stream $ s.onSucceded ()
                  onError stream s.onFailed
              } ctx
        ]

