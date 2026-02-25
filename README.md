# Atualizador de Cotações PTAX BACEN para SAP

Este projeto fornece uma solução automatizada, desenvolvida em ABAP utilizando a arquitetura **Clean Core**, para a integração e importação diária de taxas de câmbio (PTAX) disponibilizadas pelo Banco Central do Brasil (BACEN). 

A solução é compatível com as tecnologias mais recentes da SAP (ex: BTP ABAP Environment, SAP S/4HANA Cloud Public Edition ou S/4HANA Private Cloud - com suporte a Application Jobs e chamadas HTTP(S)).

O escopo padrão do projeto realiza a busca das moedas **Dólar (USD)** e **Euro (EUR)**, seleciona a cotação de fechamento diária mais adequada e atualiza de forma automatizada a infraestrutura de taxas de câmbio (exchange rates) standard do SAP.

---

## 🏗️ Arquitetura e Componentes da Solução

O desenvolvimento foi estruturado seguindo as boas práticas do Clean Code, adotando a separação de responsabilidades em componentes coesos e desacoplados:

*   **`zcl_bcb_ptax_client` (Cliente de Comunicação HTTP):** Responsável exclusivamente pela orquestração da requisição HTTP(S) no formato OData para a API pública do Banco Central do Brasil.
*   **`zcl_bcb_rate_selector` (Seletor de Cotações):** A API do BACEN disponibiliza múltiplos boletins intradiários. Esta classe isola a regra de negócio para selecionar a cotação apropriada para contabilização (tipicamente, o último boletim oficial do dia).
*   **`zcl_bcb_rate_validator` (Validador de Integridade):** Assegura a consistência e a validade dos dados em trânsito. Evita, por exemplo, a gravação de taxas nulas, formatadas incorretamente ou zeradas nos registros financeiros do SAP.
*   **`zcl_bcb_exchange_rates` (Orquestrador / Controller):** É o componente principal que invoca todos os outros em sequência lógica. Ele implementa as interfaces standard da SAP voltadas ao processamento em background (Jobs) e integração via console:
    *   `IF_APJ_RT_EXEC_OBJECT` e `IF_APJ_DT_EXEC_OBJECT`: Interfaces que capacitam a classe para ser executada e agendada pelos aplicativos do Fiori (Application Jobs).
    *   `IF_OO_ADT_CLASSRUN`: Interface que permite ao desenvolvedor invocar e debugar a execução diretamente no painel de console do Eclipse (ADT - ABAP Development Tools).
    *   Possui a responsabilidade final de instanciar a classe standard (como a `cl_exchange_rates`) e comandar a inserção no banco de dados da SAP.

### Fallback (Resiliência Operacional)
O sistema incorpora tratamento de fallback desenhado para lidar com restrições e feriados bancários — que acarretam a ausência momentânea de fechamentos das moedas. Se o processo for acionado nesses cenários não úteis ou diante de falhas momentâneas, ocorre a iteração contínua retroativa buscando obter a última taxa viável do fechamento útil anterior.

---

## 🚀 Instalação e Implantação (Deployment)

A importação dos objetos (classes e interfaces) deve ser realizada via **abapGit**.

### 1. Importação via abapGit
1. Acesse seu ambiente de desenvolvimento SAP via SAP GUI (transação `ZABAPGIT` ou `ABAPGIT`) ou utilize o plugin [abapGit para Eclipse (ADT)](https://eclipse.abapgit.org/).
2. Inicie o processo de **New Online Repository**.
3. Forneça a URL deste repositório Git.
4. Especifique o nome do Pacote (Package) ABAP onde o código será hospedado (ex: `Z_BCB_INTEGRATION`). Se o pacote não existir, o sistema solicitará a sua criação. Indique ou crie uma **Task / Transport Request** do tipo Workbench.
5. Inicie o comando **Pull** (ou **Clone**).
6. Após a conclusão do download do código para o servidor do SAP, acesse o pacote criado/definido e certifique-se de **Ativar todos os objetos importados** (Mass Activation - `Ctrl+Shift+F3`). Se houver erros de ativação temporal/dependência de classe durante este processo, reative repetidamente até que os erros desapareçam (este é o comportamento normal de ativação de pacote no Netweaver/ABAP).

---

## ⚙️ Configuração Automática (Application Job Scheduling)

Para que o SAP busque automaticamente essas taxas de câmbio todos os dias é preciso estruturar um **Job**. O Agendamento no cenário "Clean Core/Cloud" adota o catálogo e o template base formados pelo desenvolvedor.

### Passo 1: Criação dos Metadados (Catálogos e Modelos) de Job no Eclipse (ADT)

Se você for um desenvolvedor, realize essa etapa de preparação antes da entrega para o key-user usar no Fiori:

1.  No Eclipse (ADT), na árvore Project Explorer, clique com o botão direito no Pacote ABAP criado para o projeto e vá em **New > Other ABAP Repository Object**.
2.  Busque por **Application Job Catalog Entry**.
3.  Defina um Nome (ex: `Z_JC_BCB_RATES`) e uma descrição.
4.  No campo de conteúdo principal **Class Name**, referencie a classe orquestradora: `ZCL_BCB_EXCHANGE_RATES`.
5.  Salve, associe-o a uma Transport Request e **Ative** o objeto (`Ctrl+F3`).
6.  Clique com o botão direito novamente ou pressione `Ctrl+N` para procurar e criar o segundo requisito, um **Application Job Template**.
7.  Dê-lhe o nome de (ex: `Z_JT_BCB_RATES`).
8.  No campo dentro dele chamado **Catalog Entry Name**, referencie o Catálogo que criamos no sub-item 3 (`Z_JC_BCB_RATES`).
9.  Salve e **Ative** este modelo (`Ctrl+F3`).

### Passo 2: O Agendamento Automático (SAP Fiori)

Com os objetos de banco e classes expostas como Jobs configuráveis, qualquer usuário autorizado, Key-User ou Admin da base SAP, pode entrar no Fiori e disparar o relógio:

1.  Acesse o portal do **SAP Fiori Launchpad**.
2.  Pesquise e inicie o aplicativo padrão **Job de Aplicação** (Application Jobs).
3.  No topo ou rodapé da tela, clique para **Criar** (Create) um novo Job.
4.  No formulário de criação, o primeiro campo é o modelo (*Job Template*). Escolha o modelo que o desenvolvedor ativou (ex: `Z_JT_BCB_RATES`).
5.  Dê um nome para a execução da tarefa (Ex: `Automação Diária - Cotações BACEN`).
6.  Abra a seção **Opções de Programação** (Scheduling Options):
    *   **Período Inicial:** Coloque a data atual com um horário fim de tarde ou inicial noturno (Ex: **18:00**), certificando-se de que os balanços diários efetuados pelo BACEN do dia em curso foram consolidados em definitivo.
    *   **Padrão de Repetição (Recorrência):** Selecione a opção **Diariamente** (Daily).
7.  Conclua o relógio clicando no botão final **Programar** (Schedule).

Assim, aos finais da tarde, o SAP será abastecido automaticamente com a taxa atualizada.

---

## 🛠️ Execução Independente / Teste Técnico

O projeto permite o acionamento em Sandbox ou por vontade puramente de validação unitária sem comprometer filas de Jobs programados:

1. Abra a classe central `zcl_bcb_exchange_rates` na sua sessão particular do Eclipse ADT.
2. Com o mouse posicionado sobre o código livre em tela, pressione `F9` (**Run as ABAP Application Console**).
3. O painel inferior do seu ambiente exibirá instantaneamente as impressões (Outlays) de requisição, conversões de dias, sucessos (Check mark) e o registro validado dos valores do banco de dados relacional.
