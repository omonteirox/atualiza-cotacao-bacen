# Atualizador de Cotações PTAX BACEN para SAP

Solução automatizada desenvolvida em ABAP (arquitetura **Clean Core**) para integração e importação diária de taxas de câmbio PTAX do Banco Central do Brasil (BACEN) nas tabelas standard de câmbio do SAP.

Compatível com **SAP S/4HANA Cloud Public Edition** e **SAP BTP ABAP Environment** (com suporte a Communication Scenarios, Application Jobs e chamadas HTTP/S outbound).

O escopo padrão busca as cotações de **Dólar (USD)** e **Euro (EUR)**, seleciona a melhor cotação de fechamento do dia e persiste automaticamente as taxas (direta e indireta) via `cl_exchange_rates=>put`.

---

## Índice

1. [Arquitetura e Componentes](#️-arquitetura-e-componentes)
2. [Pré-requisitos](#-pré-requisitos)
3. [Instalação via abapGit](#-passo-1--instalação-via-abapgit)
4. [Communication Scenario e Outbound Service](#-passo-2--communication-scenario-e-outbound-service)
5. [Communication Arrangement](#-passo-3--communication-arrangement)
6. [Application Job (Catalog + Template)](#-passo-4--application-job-catalog--template)
7. [Business Catalog e Business Role](#-passo-5--business-catalog-e-business-role)
8. [Agendamento do Job no Fiori](#-passo-6--agendamento-do-job-no-fiori)
9. [Execução Manual / Testes](#-execução-manual--testes)

---

## 🏗️ Arquitetura e Componentes

| Componente | Tipo | Responsabilidade |
|---|---|---|
| `zif_bcb_ptax_api_client` | Interface | Contrato de abstração do cliente HTTP. Permite injeção de dependência e mock em testes unitários. |
| `zcl_bcb_ptax_api_client` | Classe | Implementação do cliente HTTP. Consome a API OData pública do BACEN via `cl_http_destination_provider=>create_by_comm_arrangement`. Usa `retry_execute` para resiliência. |
| `zif_bcb_rates_selector` | Interface | Contrato de seleção de cotação. |
| `zcl_bcb_rates_selector` | Classe | Lógica de negócio para selecionar a melhor cotação dentre os múltiplos boletins intradiários retornados pela API (ex: Abertura, Intermediário, Fechamento). |
| `zcl_bcb_rates_validator` | Classe | Validação de integridade dos dados em trânsito. Rejeita cotações nulas, zeradas ou inconsistentes. |
| `zcl_bcb_rates_orchestrator` | Classe (principal) | Orquestrador central. Implementa `IF_APJ_RT_EXEC_OBJECT`, `IF_APJ_DT_EXEC_OBJECT` (Application Job) e `IF_OO_ADT_CLASSRUN` (Console ADT). Coordena busca, seleção, validação e persistência via `cl_exchange_rates`. |
| `ztcl_bcb_rates_orchestrator` | Classe de Teste | Testes unitários ABAP Unit com mocks injetados. |

### Fallback (Resiliência)
Quando executado em dias não úteis (finais de semana ou feriados bancários), o sistema itera retroativamente até encontrar a última cotação válida (limite configurável de 5 dias úteis anteriores).

---

## 📋 Pré-requisitos

- Acesso de desenvolvedor ao SAP BTP ABAP Environment ou S/4HANA Cloud (com ADT / Eclipse).
- Plugin **abapGit** instalado no Eclipse (ADT) ou acesso à transação `ZABAPGIT` via SAP GUI.
- Permissão para criar objetos no namespace Z/Y (pacotes, classes, Communication Scenarios, etc.).
- Acesso administrativo ao SAP Fiori Launchpad para criação de Communication Arrangements, Business Catalogs e Business Roles.

---

## 📥 Passo 1 — Instalação via abapGit

1. No **Eclipse (ADT)**, abra a perspectiva **abapGit Repositories** (`Window > Show View > Other > abapGit Repositories`).
2. Clique em **"+"** (Link abapGit Repository) ou equivalente no SAP GUI.
3. Informe a **URL do repositório Git** deste projeto.
4. No campo **Package**, informe ou crie o pacote ABAP de destino (ex: `Z_BCB_INTEGRATION`).
   - Se o pacote não existir, o sistema solicitará a criação. Defina:
     - **Software Component**: `HOME` (ou o componente adequado ao seu landscape).
     - **Transport Layer**: Conforme política do seu ambiente.
   - Selecione ou crie uma **Transport Request** do tipo Workbench.
5. Execute o **Pull** (ou **Clone**) para importar todos os objetos.
6. Após o download, realize a **ativação em massa**:
   - Selecione todos os objetos do pacote → `Ctrl+Shift+F3` (Mass Activation).
   - Caso ocorram erros de dependência circular na primeira tentativa, **reative** novamente — é comportamento esperado do ABAP Activation Framework quando há interfaces e classes interdependentes.
7. Confirme que todos os objetos estão ativos (ícone verde) na árvore do Project Explorer.

---

## 🔌 Passo 2 — Communication Scenario e Outbound Service

O sistema utiliza `cl_http_destination_provider=>create_by_comm_arrangement` para obter o destino HTTP. Para isso, é necessário criar um **Communication Scenario** com um **Outbound Service** no ADT.

> **Nota:** Os IDs definidos no código-fonte são:
> - Communication Scenario: `YY1_AUTOMATIC_RATES`
> - Outbound Service ID: `YY1_ZBCB_PTAX_HTTP_REST`
>
> Você pode alterar esses valores, mas precisará ajustar as constantes `gc_comm_scenario` e `gc_service_id` na classe `zcl_bcb_ptax_api_client`.

### 2.1 Criar o Outbound Service

1. No **Eclipse (ADT)**, clique com o botão direito no pacote do projeto → **New > Other ABAP Repository Object**.
2. Busque por **Outbound Service** (categoria *Communication Management*).
3. Preencha:
   - **Name**: `YY1_ZBCB_PTAX_HTTP_REST`
   - **Description**: `BCB PTAX OData API - HTTP Outbound`
   - **Service Type**: `HTTP`
4. Salve e ative (`Ctrl+F3`).

### 2.2 Criar o Communication Scenario

1. Novamente no pacote → **New > Other ABAP Repository Object**.
2. Busque por **Communication Scenario** (categoria *Communication Management*).
3. Preencha:
   - **Name**: `YY1_AUTOMATIC_RATES`
   - **Description**: `Integração PTAX BACEN - Atualização Automática de Câmbio`
4. Com o Communication Scenario aberto no editor:
   - Vá na aba **Outbound**.
   - Clique em **Add** e selecione o Outbound Service criado no passo anterior (`YY1_ZBCB_PTAX_HTTP_REST`).
   - Em **Supported Authentication Methods**, marque **None (unauthenticated)** — a API pública do BACEN não requer autenticação.
5. Salve e ative (`Ctrl+F3`).

---

## 🌐 Passo 3 — Communication Arrangement

O Communication Arrangement é a "instância configurada" do Scenario, onde você define o host real (URL da API do BACEN). Essa configuração é feita no **SAP Fiori**, não no ADT.

1. Acesse o **SAP Fiori Launchpad**.
2. Abra o aplicativo **Communication Arrangements** (Acordos de Comunicação).
3. Clique em **New** (Criar).
4. No campo **Scenario**, selecione o Communication Scenario ativado: `YY1_AUTOMATIC_RATES`.
5. No campo **Arrangement Name**, defina um nome descritivo (ex: `BCB_PTAX_RATES`).
6. Na seção **Communication System**:
   - Clique em **New** para criar um novo Communication System (se ainda não existir), ou selecione um existente.
   - Na criação do Communication System:
     - **System ID**: `BCB_PTAX` (ou nome de sua preferência)
     - **Host Name**: `olinda.bcb.gov.br`
     - **Port**: `443`
     - **No Authentication**: Habilitado (a API é pública).
   - Salve o Communication System e retorne à tela do Arrangement.
7. Na seção **Outbound Services**, o serviço `YY1_ZBCB_PTAX_HTTP_REST` deverá aparecer automaticamente.
   - No campo **Path**, informe o caminho base da API OData:
     ```
     /olinda/servico/PTAX/versao/v1/odata/
     ```
   - Marque **Service Status** como **Active**.
8. Clique em **Save** (Salvar).

> **Validação rápida**: Após salvar, você pode clicar em **Check Connection** para verificar se o SAP consegue alcançar o host `olinda.bcb.gov.br` na porta 443.

---

## ⏰ Passo 4 — Application Job (Catalog + Template)

### 4.1 Criar o Application Job Catalog Entry

1. No **Eclipse (ADT)**, clique com o botão direito no pacote → **New > Other ABAP Repository Object**.
2. Busque por **Application Job Catalog Entry** (categoria *Application Jobs*).
3. Preencha:
   - **Name**: `Z_JC_BCB_RATES`
   - **Description**: `Catálogo - Atualização PTAX BACEN`
4. No campo **Class Name**, informe a classe orquestradora: `ZCL_BCB_RATES_ORCHESTRATOR`.
5. Salve, associe a uma Transport Request e ative (`Ctrl+F3`).

### 4.2 Criar o Application Job Template

1. No pacote → **New > Other ABAP Repository Object** → **Application Job Template**.
2. Preencha:
   - **Name**: `Z_JT_BCB_RATES`
   - **Description**: `Template - Atualização PTAX BACEN`
3. No campo **Catalog Entry Name**, referencie o catálogo criado: `Z_JC_BCB_RATES`.
4. Salve e ative (`Ctrl+F3`).

---

## 🔐 Passo 5 — Business Catalog e Business Role

Para que o Job Template fique visível e utilizável no aplicativo **Application Jobs** do SAP Fiori, ele precisa estar vinculado a um **Business Catalog**, que por sua vez deve estar atribuído a uma **Business Role**.

### 5.1 Vincular ao Business Catalog

1. No **Eclipse (ADT)**, abra ou crie um **Business Catalog** adequado à área financeira (ex: `Z_BC_FIN_RATES`).
2. No editor do Business Catalog, acesse a aba **Apps**.
3. Clique em **Add** e selecione o Application Job Template `Z_JT_BCB_RATES`.
4. Salve e ative (`Ctrl+F3`).

### 5.2 Atribuir à Business Role

1. No **SAP Fiori Launchpad**, abra o aplicativo **Maintain Business Roles**.
2. Abra a Business Role que será utilizada pelos usuários responsáveis (ex: `Z_BR_FIN_ADMIN`), ou crie uma nova.
3. Na aba **Assigned Business Catalogs**, clique em **Add** e selecione o catálogo `Z_BC_FIN_RATES`.
4. Salve a Business Role.
5. Certifique-se de que os **usuários** responsáveis pelo agendamento e monitoramento estão atribuídos a essa Business Role (aba **Assigned Business Users**).

---

## 📅 Passo 6 — Agendamento do Job no Fiori

Com toda a infraestrutura configurada, o Job pode ser agendado:

1. Acesse o **SAP Fiori Launchpad**.
2. Abra o aplicativo **Application Jobs** (Job de Aplicação).
3. Clique em **Create** (Criar).
4. No campo **Job Template**, selecione: `Z_JT_BCB_RATES`.
5. No campo **Job Name**, defina um nome descritivo (ex: `Atualização Diária PTAX BACEN`).
6. Na seção **Scheduling Options** (Opções de Programação):
   - **Start Date/Time**: Data atual, horário recomendado: **18:00** (após o fechamento dos boletins do BACEN).
   - **Recurrence Pattern**: Selecione **Daily** (Diariamente).
7. Clique em **Schedule** (Programar).

O sistema executará diariamente no horário configurado, buscando as cotações do dia e persistindo no SAP. A execução pode ser monitorada no mesmo aplicativo **Application Jobs**, na aba de histórico de execuções.

---

## 🛠️ Execução Manual / Testes

### Via Console ADT (Desenvolvedor)
Para validação rápida sem agendamento:

1. Abra a classe `zcl_bcb_rates_orchestrator` no Eclipse (ADT).
2. Pressione `F9` (**Run as ABAP Application Console**).
3. O console exibirá o log completo: requisições HTTP, cotações selecionadas, validações e resultado da gravação no SAP.

### Via Application Jobs (Execução Única)
1. No aplicativo **Application Jobs** do Fiori, crie um Job com o template `Z_JT_BCB_RATES`.
2. Em **Scheduling Options**, selecione **Immediate** (Imediato) ao invés de agendar para um horário futuro.
3. Clique em **Schedule**. O Job será disparado imediatamente.

---

## 📂 Estrutura do Repositório

```
src/
├── zcl_bcb_ptax_api_client.clas.abap      # Cliente HTTP (API BACEN)
├── zcl_bcb_rates_selector.clas.abap       # Seletor de melhor cotação
├── zcl_bcb_rates_validator.clas.abap      # Validador de integridade
├── zcl_bcb_rates_orchestrator.clas.abap   # Orquestrador principal (Job + Console)
├── ztcl_bcb_rates_orchestrator.clas.abap  # Testes unitários (ABAP Unit)
├── zif_bcb_ptax_api_client.intf.abap      # Interface do cliente HTTP
├── zif_bcb_rates_selector.intf.abap       # Interface do seletor
└── *.xml                                  # Metadados dos objetos ABAP
```
