import json from '../../../globalVariables.json';

describe('Create Appointment Test', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
    cy.visit('http://localhost:4200/staff/appointment/update');
  });

    it('Visits the Appointment Update Component', () => {
      cy.contains('h3', 'Select an Appointment to Update');
    });

  it('should display a list of appointments', () => {
    cy.contains('Select an Appointment to Update').should('be.visible');

    cy.get('.appointment-item').should('have.length', 1);

  });
});
