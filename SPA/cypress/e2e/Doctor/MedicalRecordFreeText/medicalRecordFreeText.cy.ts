import json from '../../../globalVariables.json';


describe('Medical Record Free Text Tests',()=> {

    beforeEach(() => {
      cy.clearCookies();
      cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
      cy.visit('/staff/patients/medicalRecord');
    });

  it('Visits the Medical Record Management Component', () => {
    cy.contains('h1', 'Medical Record Management');
  });

  it("Adds a Free Text Comment", () => {

    cy.get(".medical-record-free-text-details .medical-record-free-text-item").then(($items) => {
      const initialSize = $items.length;

      cy.get(".add-button").click();

      const comment = "This patient only has one teeth :(";

      cy.get("#comment").type(comment);
      cy.get('button').click();

      cy.wait(1000);
      cy.visit('/staff/patients/medicalRecord');
      cy.reload();

      cy.log(initialSize + "");
      cy.get(".medical-record-free-text-details .medical-record-free-text-item")
        .should("have.length", (initialSize + 1));
    });


    cy.get(".medical-record-free-text-details .medical-record-free-text-item").then(($items) => {
      const initialSize = $items.length;

      cy.get(".add-button").click();

      const comment = "This patient only has one kidney";

      cy.get("#comment").type(comment);
      cy.get('button').click();

      cy.wait(1000);
      cy.visit('/staff/patients/medicalRecord');
      cy.reload();

      cy.log(initialSize + "");
      cy.get(".medical-record-free-text-details .medical-record-free-text-item")
        .should("have.length", (initialSize + 1));
    });
  });

  })
