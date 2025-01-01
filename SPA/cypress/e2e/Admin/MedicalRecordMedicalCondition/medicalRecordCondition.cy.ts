import json from '../../../globalVariables.json';

describe('MedicalRecordMedicalCondition Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
    cy.visit('http://localhost:4200/staff/patients');

    const newName = 'new User';

    cy.contains('.patient-profile-card', newName)
      .find('.view-button')
      .click();
  });

  it('Visits the Medical Condition Management Component', () => {
    cy.contains('h1', 'Medical Record Details');
    cy.contains('app-medical-record-condition-list > .sub-header', 'Medical Conditions');
  });

  it('Verify the list of Medical Conditions is not empty', () => {
    //this user has two medical conditions added in the bootstrap data
    cy.get('.medical-record-condition-item').should('have.length', 2);
  });

});
