import json from '../../../globalVariables.json';

describe('Allergy Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Doctor);
    cy.visit('/staff/searchAllergy');
  });

  it('Visits the Allergy Management Component', () => {
    cy.contains('h1', 'Allergy Management');
  });

  it('Filter Allergy', ()=>{
    const code='AL10';
    cy.get('.filter-input').click();
    cy.get('.filter-options > :nth-child(1)').click();
    cy.get('#name-input').type(code);
    cy.get('.modal-actions > :nth-child(1)').click();
    cy.get('.allergy-list .allergy-item').should('have.length.at.least', 1);

  });


});
