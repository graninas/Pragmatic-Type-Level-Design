# Recommended Design For HyperSwitch

This demo project showcases an extensible and highly configurable architecture of the payment orchestrator domain.

**Features:**

- Proper architecture with a clear separation of layers
- Extensible payment methods
- Extensible payment processors
- Each specific payment processor can have own communication data
- Flows that are optimal for reading and understanding (see `GenericPaymentCreateFlowTemplate::execute`)
- Templates of flows (with the Template Method pattern)
- Concrete flows specify template flows
- Each specific flow has own payment data type and result data type (with associated types)
- Flows can be constructed and used as first-class citizens (see `flow_construction_test.rs`)
- Flows can be configured
- Flows can wrap other flows and provide extra features (see `SimplePaymentCreateFlow` and `LoggingPaymentCreateFlow`)
- External dependencies are separated (see `domain::services` and `application::services`
- Basic validation eDSL that helps with flow construction (see `flow_construction_test.rs`)

**Additionally**

The project contains two showcases of a type-level approach to flow construction and payment processors extensibility. These showcases don't participate in the main application.

- Flow construction type-level eDSL: `tl_flow_edsl.rs`
- Payment processors and methods extensible declarative type-level eDSL: `tl_payment_processor_edsl.rs`
