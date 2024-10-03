      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RESCDSA1.
       AUTHOR.     DANIELA.

      *================================================================*
      *                T  R  E  I  N  A  M  E  N  T  O                 *
      *================================================================*
      *    PROGRAMA....: RESCDSA1                                      *
      *    PROGRAMADOR.: DANIELA LUCIANO       - TREINAMENTO           *
      *    ANALISTA....: RENATA  ROTIROTI      - TREINAMENTO           *
      *    DATA........: 25/02/2019                                    *
      *----------------------------------------------------------------*
      *   OBJETIVO....: COMPARAR O ARQUIVO DE ENTRADA EARQREST COM O   *
      * ARQUIVO DE ENTRADA EARQLIGC, SE SUAS CHAVES FOREM IGUAIS SALVAR*
      * O REGISTRO DO EARQREST NO ARQUIVO DE SAIDA SARQREST E OS CAMPOS*
      * DO EARQLIGC CPF-CNPJ-LIG, CFLIAL-LIG, CTRL-LIG, TIPO-IMPED,    *
      * QTDE-IMPED, QTDE-IMPED, VALOR-TOT-IMPED, DTA-ULT-OCORR-IMP NO  *
      * ARQUIVO DE SAIDA SARQLIGC.                                     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME            I/O                   INCLUDE/BOOK      *
      *     EARQREST            I                      RESCWRES        *
      *     EARQLIGC            I                      RESCWLIG        *
      *     SARQREST            O                      RESTWDSR        *
      *     SARQLIGC            O                      RESTWDSL        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA PARA GRAVACAO DE ERROS                      *
      *    RESCWRES - LAYOUT DO ARQUIVO DE ENTRADA RESTRICOES          *
      *    RESCWLIG - LAYOUT DO ARQUIVO DE ENTRADA LIGACOES            *
      *    RESTWDSR - LAYOUT DO ARQUIVO DE SAIDA RESTRICOES            *
      *    RESTWDSL - LAYOUT DO ARQUIVO DE SAIDA LIGACOES              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERROS                              *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EARQREST ASSIGN      TO UT-S-EARQREST
                      FILE STATUS      IS WRK-FS-EARQREST.

           SELECT EARQLIGC ASSIGN      TO UT-S-EARQLIGC
                      FILE STATUS      IS WRK-FS-EARQLIGC.

           SELECT SARQREST ASSIGN      TO UT-S-SARQREST
                      FILE STATUS      IS WRK-FS-SARQREST.

           SELECT SARQLIGC ASSIGN      TO UT-S-SARQLIGC
                      FILE STATUS      IS WRK-FS-SARQLIGC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - DADOS DO ARQUIVO DE ENTRADA(EARQREST)               *
      *                                -  LRECL   = 062                *
      *----------------------------------------------------------------*

       FD  EARQREST
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQREST                 PIC  X(062).

      *----------------------------------------------------------------*
      *    INPUT - DADOS DO ARQUIVO DE ENTRADA(EARQLIGC)               *
      *                                -  LRECL   = 112                *
      *----------------------------------------------------------------*

       FD  EARQLIGC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EARQLIGC                 PIC  X(112).

      *----------------------------------------------------------------*
      *    OUTPUT - DADOS DO ARQUIVO DE SAIDA (SARQREST)               *
      *                                -  LRECL   = 090                *
      *----------------------------------------------------------------*

       FD  SARQREST
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SARQREST                 PIC  X(090).

      *----------------------------------------------------------------*
      *    OUTPUT - DADOS DO ARQUIVO DE SAIDA (SARQLIGC)               *
      *                                -  LRECL   = 050                *
      *----------------------------------------------------------------*

       FD  SARQLIGC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SARQLIGC                 PIC  X(050).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING RESCDSA1 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-EARQREST          PIC  9(009) COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-EARQLIGC          PIC  9(009) COMP-3  VALUE ZEROS.
       01  ACU-GRAVS-SARQREST          PIC  9(009) COMP-3  VALUE ZEROS.
       01  ACU-GRAVS-SARQLIGC          PIC  9(009) COMP-3  VALUE ZEROS.
       01  ACU-INCONS-EARQREST         PIC  9(009) COMP-3  VALUE ZEROS.
       01  ACU-INCONS-EARQLIGC         PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-EARQREST             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EARQLIGC             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SARQREST             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SARQLIGC             PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO DE ENTRADA EARQREST ***'.
      *----------------------------------------------------------------*

           COPY RESCWRES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO DE ENTRADA EARQLIGC ***'.
      *----------------------------------------------------------------*

           COPY RESCWLIG.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO DE SAIDA SARQREST***'.
      *----------------------------------------------------------------*

           COPY RESTWDSR.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO ARQUIVO DE SAIDA SARQLIGC***'.
      *----------------------------------------------------------------*

           COPY RESTWDSL.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       01  WRK-MASC                    PIC  ZZZ.ZZZ.ZZ9    VALUE SPACES.
       01 WRK-AUX-INCONS               PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE REDEFINES ***'.
      *----------------------------------------------------------------*

       01 WRK-AUX-S9-03                PIC  +9(003)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-03.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-03            PIC  9(003).

       01 WRK-AUX-S9-09                PIC  +9(009)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-09.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-09            PIC  9(009).

       01 WRK-AUX-S9-05                PIC  +9(005)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-05.
           05 FILLER                   PIC  9(002).
           05 WRK-AUX-09-04            PIC  9(004).

       01 WRK-AUX-S9-02                PIC  +9(002)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-02.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-02            PIC  9(002).

       01 WRK-AUX-S9-01                PIC  +9(001)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-01.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-01            PIC  9(001).

       01 WRK-AUX-S9-07                PIC  +9(007)        VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-07.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-07            PIC  9(007).

       01 WRK-AUX-S9-13                PIC  +9(013)V99     VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-13.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-13            PIC  9(013)V99.

       01 WRK-AUX-S9-15                PIC  +9(015)V99     VALUE ZEROS.
       01 FILLER                       REDEFINES         WRK-AUX-S9-15.
           05 FILLER                   PIC  9(001).
           05 WRK-AUX-09-15            PIC  9(015)V99.

       01 WRK-AUX-DATA                 PIC  X(010)         VALUE SPACES.
       01 FILLER                       REDEFINES           WRK-AUX-DATA.
           05 WRK-AUX-DIA              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-MES              PIC  9(002).
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-ANO              PIC  9(004).

       01 WRK-DATA                     PIC  9(008)         VALUE ZEROS.
       01 FILLER                       REDEFINES           WRK-DATA.
           05 WRK-ANO                  PIC  9(004).
           05 WRK-MES                  PIC  9(002).
           05 WRK-DIA                  PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CHAVES ***'.
      *----------------------------------------------------------------*

       01  WRK-CHV-EARQREST.
           05 WRK-CPF-CGC-EARQREST     PIC  9(009)         VALUE ZEROS.
           05 WRK-FILIAL-CGC-EARQREST  PIC  9(004)         VALUE ZEROS.

       01  WRK-CHV-EARQLIGC.
           05 WRK-CPF-CNPJ-EARQLIGC    PIC  9(009)         VALUE ZEROS.
           05 WRK-FILIAL-PROP-EARQLIGC PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-TEXTO.
           05 FILLER                   PIC  X(007)         VALUE
              '* ERRO '.
           05 WRK-OPERACAO             PIC  X(014)         VALUE SPACES.
           05 FILLER                   PIC  X(011)         VALUE
              'DO ARQUIVO '.
           05 WRK-ARQUIVO              PIC  X(009)         VALUE SPACES.
           05 FILLER                   PIC  X(016)         VALUE
              '- FILE-STATUS = '.
           05 WRK-FS                   PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(002)         VALUE
              ' *'.

       01  WRK-MENSAGEM.
           05 WRK-ABERTURA             PIC  X(013)         VALUE
             'NA ABERTURA'.
           05 WRK-LEITURA              PIC  X(013)         VALUE
             'NA LEITURA'.
           05 WRK-GRAVACAO             PIC  X(013)         VALUE
             'NA GRAVACAO'.
           05 WRK-FECHAMENTO           PIC  X(013)         VALUE
             'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA TRATAMENTO ERRO ***'.
      *----------------------------------------------------------------*

           COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RESCDSA1 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    ROTINA PRINCIPAL                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR

           PERFORM 2000-VERIFICAR-VAZIO

           PERFORM 3000-PROCESSAR
               UNTIL WRK-CHV-EARQREST  EQUAL HIGH-VALUES
                 OR  WRK-CHV-EARQLIGC  EQUAL HIGH-VALUES

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EARQREST
                       EARQLIGC
                OUTPUT SARQREST
                       SARQLIGC

           MOVE WRK-ABERTURA           TO WRK-OPERACAO

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EARQREST

           PERFORM 1120-TESTAR-FS-EARQLIGC

           PERFORM 1130-TESTAR-FS-SARQREST

           PERFORM 1140-TESTAR-FS-SARQLIGC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA - EARQREST         *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-EARQREST         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EARQREST          NOT EQUAL ZEROS
              MOVE 'EARQREST'          TO WRK-ARQUIVO
              MOVE WRK-FS-EARQREST     TO WRK-FS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE '0010'              TO ERR-LOCAL
              MOVE WRK-TEXTO           TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO DE ENTRADA - EARQLIGC         *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-EARQLIGC         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EARQLIGC          NOT EQUAL ZEROS
              MOVE 'EARQLIGC'          TO WRK-ARQUIVO
              MOVE WRK-FS-EARQLIGC     TO WRK-FS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE '0020'              TO ERR-LOCAL
              MOVE WRK-TEXTO           TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO DE SAIDA - SARQREST           *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SARQREST         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-SARQREST          NOT EQUAL ZEROS
              MOVE 'SARQREST'          TO WRK-ARQUIVO
              MOVE WRK-FS-SARQREST     TO WRK-FS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE '0030'              TO ERR-LOCAL
              MOVE WRK-TEXTO           TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO DE SAIDA - SARQLIGC           *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-SARQLIGC         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-SARQLIGC          NOT EQUAL ZEROS
              MOVE 'SARQLIGC'          TO WRK-ARQUIVO
              MOVE WRK-FS-SARQLIGC     TO WRK-FS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE '0030'              TO ERR-LOCAL
              MOVE WRK-TEXTO           TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR ARQUIVO VAZIO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EARQREST

           IF WRK-FS-EARQREST          EQUAL '10'
              DISPLAY '***************** RESCDSA1 ******************'
              DISPLAY '*                                           *'
              DISPLAY '*           ARQUIVO EARQREST VAZIO          *'
              DISPLAY '*                                           *'
              DISPLAY '***************** RESCDSA1 ******************'
              PERFORM 4000-FINALIZAR
           END-IF

           PERFORM 2200-LER-EARQLIGC

           IF WRK-FS-EARQLIGC          EQUAL '10'
              DISPLAY '***************** RESCDSA1 ******************'
              DISPLAY '*                                           *'
              DISPLAY '*           ARQUIVO EARQLIGC VAZIO          *'
              DISPLAY '*                                           *'
              DISPLAY '***************** RESCDSA1 ******************'
              PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE ENTRADA - EARQREST                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-EARQREST               SECTION.
      *----------------------------------------------------------------*

           READ EARQREST               INTO RESCWRES-REG-EARQREST

           IF WRK-FS-EARQREST          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHV-EARQREST
              GO                       TO 2100-99-FIM
           END-IF

           MOVE WRK-LEITURA            TO WRK-OPERACAO

           PERFORM 1110-TESTAR-FS-EARQREST

           ADD 1                       TO ACU-LIDOS-EARQREST.

           PERFORM 2110-CONSISTIR-DADOS-EARQREST

           IF WRK-AUX-INCONS           EQUAL 'S'
              ADD 1                    TO ACU-INCONS-EARQREST
              GO                       TO 2100-LER-EARQREST
           ELSE
              MOVE RESCWRES-CPF-CGC       TO WRK-AUX-S9-09
              MOVE WRK-AUX-09-09          TO WRK-CPF-CGC-EARQREST
              MOVE RESCWRES-FILIAL-CGC    TO WRK-AUX-S9-05
              MOVE WRK-AUX-09-04          TO WRK-FILIAL-CGC-EARQREST
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTIR DADOS DO ARQUIVO DE ENTRADA - EARQREST            *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONSISTIR-DADOS-EARQREST   SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-AUX-INCONS

           IF (RESCWRES-TIPO-IMPED     NOT NUMERIC) OR
              (RESCWRES-TIPO-IMPED     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-CPF-CGC        NOT NUMERIC) OR
              (RESCWRES-CPF-CGC        EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-FILIAL-CGC     NOT NUMERIC) OR
              (RESCWRES-FILIAL-CGC     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-CTRL-CPF       NOT NUMERIC) OR
              (RESCWRES-CTRL-CPF       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-MAIOR-GRAU     NOT NUMERIC) OR
              (RESCWRES-MAIOR-GRAU     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-QTDE-IMPED     NOT NUMERIC) OR
              (RESCWRES-QTDE-IMPED     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-VALOR-MIN-IMPED
                                       NOT NUMERIC) OR
              (RESCWRES-VALOR-MIN-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-VALOR-MAX-IMPED
                                       NOT NUMERIC) OR
              (RESCWRES-VALOR-MAX-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF (RESCWRES-VALOR-TOT-IMPED
                                       NOT NUMERIC) OR
              (RESCWRES-VALOR-TOT-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF RESCWRES-DTA-PRI-OCORR-IMPED
                                       EQUAL SPACES OR LOW-VALUES
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF

           IF RESCWRES-DTA-ULT-OCORR-IMPED
                                       EQUAL SPACES OR LOW-VALUES
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2110-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE ENTRADA - EARQLIGC                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-EARQLIGC               SECTION.
      *----------------------------------------------------------------*

           READ EARQLIGC               INTO RESCWLIG-REG-EARQLIGC

           IF WRK-FS-EARQLIGC          EQUAL '10'
              MOVE HIGH-VALUES         TO WRK-CHV-EARQLIGC
              GO                       TO 2200-99-FIM
           END-IF

           MOVE WRK-LEITURA            TO WRK-OPERACAO

           PERFORM 1120-TESTAR-FS-EARQLIGC

           ADD 1                       TO ACU-LIDOS-EARQLIGC

           PERFORM 2210-CONSISTIR-DADOS-EARQLIGC

           IF WRK-AUX-INCONS           EQUAL 'S'
              ADD 1                    TO ACU-INCONS-EARQLIGC
              GO                       TO 2200-LER-EARQLIGC
           ELSE
              MOVE CPF-CNPJ-PROP
                                       TO WRK-CHV-EARQLIGC
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTIR DADOS DO ARQUIVO DE ENTRADA - EARQLIGC            *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-CONSISTIR-DADOS-EARQLIGC   SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-AUX-INCONS

           IF (RESCWLIG-CTPO-REG       NOT NUMERIC) OR
              (RESCWLIG-CTPO-REG       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CPF-CNPJ-PROP  NOT NUMERIC) OR
              (RESCWLIG-CPF-CNPJ-PROP  EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-FILIAL-PROP    NOT NUMERIC) OR
              (RESCWLIG-FILIAL-PROP    EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CCPF-CNPJ-LIG  NOT NUMERIC) OR
              (RESCWLIG-CCPF-CNPJ-LIG  EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CFLIAL-LIG     NOT NUMERIC) OR
              (RESCWLIG-CFLIAL-LIG     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-TIPO-IMPED     NOT NUMERIC) OR
              (RESCWLIG-TIPO-IMPED     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-MAIOR-GRAU     NOT NUMERIC) OR
              (RESCWLIG-MAIOR-GRAU     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CTRL-PROP      NOT NUMERIC) OR
              (RESCWLIG-CTRL-PROP      EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CTRL-LIG       NOT NUMERIC) OR
              (RESCWLIG-CTRL-LIG       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-QTDE-IMPED     NOT NUMERIC) OR
              (RESCWLIG-QTDE-IMPED     EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-VALOR-MIN-IMPED
                                       NOT NUMERIC) OR
              (RESCWLIG-VALOR-MIN-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-VALOR-MAX-IMPED
                                       NOT NUMERIC) OR
              (RESCWLIG-VALOR-MAX-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-VALOR-TOT-IMPED
                                       NOT NUMERIC) OR
              (RESCWLIG-VALOR-TOT-IMPED
                                       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF RESCWLIG-DTA-PRI-OCORR-IMPED
                                       EQUAL SPACES OR LOW-VALUES
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF RESCWLIG-DTA-ULT-OCORR-IMPED
                                       EQUAL SPACES OR LOW-VALUES
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF

           IF (RESCWLIG-CTPO-LIG       NOT NUMERIC) OR
              (RESCWLIG-CTPO-LIG       EQUAL ZEROS)
              MOVE 'S'                 TO WRK-AUX-INCONS
              GO                       TO 2210-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-EARQLIGC         EQUAL WRK-CHV-EARQREST
              PERFORM 3100-TRATAR-IGUAIS
                     UNTIL  WRK-CHV-EARQLIGC NOT EQUAL WRK-CHV-EARQREST
           ELSE
              IF WRK-CHV-EARQLIGC      GREATER WRK-CHV-EARQREST
                 PERFORM 2100-LER-EARQREST
                    UNTIL WRK-CHV-EARQREST
                                       NOT LESS WRK-CHV-EARQLIGC
              ELSE
                PERFORM 2200-LER-EARQLIGC
                   UNTIL WRK-CHV-EARQLIGC
                                       NOT LESS WRK-CHV-EARQREST
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAMENTO PARA O CASO DOS REGISTROS SEREM IGUAIS             *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-IGUAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE RESCWRES-TIPO-IMPED    TO WRK-AUX-S9-03
           MOVE WRK-AUX-09-03          TO RESTWDSR-TIPO-IMPED
           MOVE WRK-CPF-CGC-EARQREST   TO RESTWDSR-CPF-CGC
           MOVE WRK-FILIAL-CGC-EARQREST
                                       TO RESTWDSR-FILIAL-CGC
           MOVE RESCWRES-CTRL-CPF      TO WRK-AUX-S9-02
           MOVE WRK-AUX-09-02          TO RESTWDSR-CTRL-CPF
           MOVE RESCWRES-MAIOR-GRAU    TO WRK-AUX-S9-01
           MOVE WRK-AUX-09-01          TO RESTWDSR-MAIOR-GRAU
           MOVE RESCWRES-QTDE-IMPED    TO WRK-AUX-S9-07
           MOVE WRK-AUX-09-07          TO RESTWDSR-QTDE-IMPED
           MOVE RESCWRES-VALOR-MIN-IMPED
                                       TO WRK-AUX-S9-13
           MOVE WRK-AUX-09-13          TO RESTWDSR-VALOR-MIN-IMPED
           MOVE RESCWRES-VALOR-MAX-IMPED
                                       TO WRK-AUX-S9-13
           MOVE WRK-AUX-09-13          TO RESTWDSR-VALOR-MAX-IMPED
           MOVE RESCWRES-VALOR-TOT-IMPED
                                       TO WRK-AUX-S9-15
           MOVE WRK-AUX-09-15          TO RESTWDSR-VALOR-TOT-IMPED
           MOVE RESCWRES-DTA-PRI-OCORR-IMPED
                                       TO WRK-AUX-DATA
           MOVE WRK-AUX-DIA            TO WRK-DIA
           MOVE WRK-AUX-MES            TO WRK-MES
           MOVE WRK-AUX-ANO            TO WRK-ANO
           MOVE WRK-DATA               TO RESTWDSR-DTA-PRI-OCORR-IMPED
           MOVE RESCWRES-DTA-ULT-OCORR-IMPED
                                       TO WRK-AUX-DATA
           MOVE WRK-AUX-DIA            TO WRK-DIA
           MOVE WRK-AUX-MES            TO WRK-MES
           MOVE WRK-AUX-ANO            TO WRK-ANO
           MOVE WRK-DATA               TO RESTWDSR-DTA-ULT-OCORR-IMPED

           PERFORM 3110-GRAVAR-SARQREST

           MOVE RESCWLIG-CCPF-CNPJ-LIG TO RESTWDSL-CCPF-CNPJ-LIG
           MOVE RESCWLIG-CFLIAL-LIG    TO RESTWDSL-CFLIAL-LIG
           MOVE RESCWLIG-CTRL-LIG      TO RESTWDSL-CTRL-LIG
           MOVE RESCWLIG-TIPO-IMPED    TO RESTWDSL-TIPO-IMPED
           MOVE RESCWLIG-QTDE-IMPED    TO RESTWDSL-QTDE-IMPED
           MOVE RESCWLIG-VALOR-TOT-IMPED
                                       TO RESTWDSL-VALOR-TOT-IMPED
           MOVE RESCWLIG-DTA-ULT-OCORR-IMPED
                                       TO WRK-AUX-DATA
           MOVE WRK-AUX-DIA            TO WRK-DIA
           MOVE WRK-AUX-MES            TO WRK-MES
           MOVE WRK-AUX-ANO            TO WRK-ANO
           MOVE WRK-DATA               TO RESTWDSL-DTA-ULT-OCORR-IMPED

           PERFORM 3120-GRAVAR-SARQLIGC

           PERFORM 2200-LER-EARQLIGC.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DE SAIDA     - SARQREST                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-GRAVAR-SARQREST            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SARQREST           FROM RESTWDSR-REG-SARQREST

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1130-TESTAR-FS-SARQREST

           ADD 1                       TO ACU-GRAVS-SARQREST.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DE SAIDA     - SARQLIGC                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-GRAVAR-SARQLIGC            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-SARQLIGC           FROM RESTWDSL-REG-SARQLIGC

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1140-TESTAR-FS-SARQLIGC

           ADD 1                       TO ACU-GRAVS-SARQLIGC.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZACAO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS

           CLOSE EARQREST
                 EARQLIGC
                 SARQREST
                 SARQLIGC

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO

           PERFORM 1100-TESTAR-FILE-STATUS

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           DISPLAY
           '*********************** RESCDSA1 ***********************'.
           DISPLAY
           '*                                                      *'.
           DISPLAY
           '*                 RESULTADO DO PROCESSAMENTO:          *'.
           DISPLAY
           '*                                                      *'.
           DISPLAY
           '* TOTAIS PROCESSADOS:                                  *'.
           DISPLAY
           '*                                                      *'.

           MOVE ACU-LIDOS-EARQREST     TO WRK-MASC

           DISPLAY
           '* REGISTROS LIDOS NO EARQREST       =>     ' WRK-MASC ' *'

           MOVE ACU-LIDOS-EARQLIGC     TO WRK-MASC

           DISPLAY
           '* REGISTROS LIDOS NO EARQLIGC       =>     ' WRK-MASC ' *'

           MOVE ACU-INCONS-EARQREST    TO WRK-MASC

           DISPLAY
           '* REGISTROS INCONSISTENTES NO EARQREST =>  ' WRK-MASC ' *'

           MOVE ACU-INCONS-EARQLIGC    TO WRK-MASC

           DISPLAY
           '* REGISTROS INCONSISTENTES NO EARQLIGC =>  ' WRK-MASC ' *'

           MOVE ACU-GRAVS-SARQREST     TO WRK-MASC

           DISPLAY
           '* REGISTROS GRAVADOS NO SARQREST    =>     ' WRK-MASC ' *'

           MOVE ACU-GRAVS-SARQLIGC     TO WRK-MASC

           DISPLAY
           '* REGISTROS GRAVADOS NO SARQLIGC    =>     ' WRK-MASC ' *'

           DISPLAY
           '********************************************************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA TRATAMENTO DE ERROS                             *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'RESCDSA1'             TO ERR-PGM

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------* 