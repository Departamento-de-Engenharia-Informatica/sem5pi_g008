import json from '../../../globalVariables.json';

describe('MedicalCondition Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Admin);
    cy.visit('/admin/medicalConditionManagement');
  });

  it('Visits the Medical Condition Management Component', () => {
    cy.contains('h1', 'Medical Conditions Management');
  });

  it('Add a new Medical Condition and verify the list size increases', () => {
    const minuteAndSecond = new Date().toISOString().slice(15, 19).replace(':', '');
    const code = `MC${minuteAndSecond}`;
    const designation = `Hypertension${minuteAndSecond}`;
    const description = 'High blood pressure';
    const symptom = 'Headache';

    cy.get('.medical-conditions-list .medical-condition-item').then((initialList) => {
      const initialCount = initialList.length;

      // Add a new medical condition
      cy.get('.add-button').click();
      cy.get('#code').type(code);
      cy.get('#designation').type(designation);
      cy.get('#description').type(description);
      cy.get('#symptom').type(symptom);
      cy.get('.symptom-input > button').click();
      cy.get('[type="submit"]').click();

      cy.get('.medical-conditions-list .medical-condition-item').should('have.length', initialCount + 1);
    });
  });

  it('Add a new Medical Condition and handle invalid code error', () => {
    const minuteAndSecond = new Date().toISOString().slice(15, 19).replace(':', '');
    const code = "MC123123123";
    const designation = `Hypertension${minuteAndSecond}`;
    const description = 'High blood pressure';
    const symptom = 'Headache';

    cy.intercept('POST', 'http://localhost:4000/api/medicalConditions', {
      statusCode: 805,
      body: {
        message: 'Invalid ICD-11 code.'
      }
    }).as('addMedicalConditionRequest');

    cy.get('.add-button').click();
    cy.get('#code').type(code);
    cy.get('#designation').type(designation);
    cy.get('#description').type(description);
    cy.get('#symptom').type(symptom);
    cy.get('.symptom-input > button').click();
    cy.get('[type="submit"]').click();

    cy.wait('@addMedicalConditionRequest').then((interception) => {
      console.log(interception);
      expect(interception.response!.statusCode).to.eq(805);
      expect(interception.response!.body.message).to.contain('Invalid ICD-11 code.');
    });
  });

  it('Add a new Medical Condition and handle with duplicate designation ', () => {
    const code = `CA12`;
    const designation = 'CANCER';
    const description = 'High blood pressure';
    const symptom = 'Headache';

    cy.intercept('POST', 'http://localhost:4000/api/medicalConditions', {
      statusCode: 500,
      body: {
        message: 'Code/Designation already exists'
      }
    }).as('addMedicalConditionRequest');

    cy.get('.add-button').click();
    cy.get('#code').type(code);
    cy.get('#designation').type(designation);
    cy.get('#description').type(description);
    cy.get('#symptom').type(symptom);
    cy.get('.symptom-input > button').click();
    cy.get('[type="submit"]').click();

    cy.wait('@addMedicalConditionRequest').then((interception) => {
      console.log(interception);
      expect(interception.response!.statusCode).to.eq(500);
      expect(interception.response!.body.message).to.contain('Code/Designation already exists');
    });
  });
});

