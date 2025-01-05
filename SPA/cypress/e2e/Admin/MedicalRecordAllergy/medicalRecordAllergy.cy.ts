import json from '../../../globalVariables.json';

describe('MedicalRecordAllergy Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
    cy.visit('http://localhost:4200/staff/patients');
  });

  it('Visits the Medical Record Allergy Management Component', () => {

    const newName = 'new User';

    cy.contains('.patient-profile-card', newName)
      .find('.view-button')
      .click();

    cy.contains('h1', 'Medical Record Details');
    cy.contains('app-medical-record-allergy-list > .sub-header', 'Allergies');
  });

  it('Verify the list of Allergies is not empty', () => {

    const newName = 'new User';

    cy.contains('.patient-profile-card', newName)
      .find('.view-button')
      .click();

    cy.get('.medical-record-allergy-item').should('have.length', 2);
  });

  it('Verify the list of Allergies is empty', () => {

    const newName = 'Alice Smith';

    cy.contains('.patient-profile-card', newName)
      .find('.view-button')
      .click();

    cy.get('.medical-record-allergy-item').should('have.length', 0);
  });

});
