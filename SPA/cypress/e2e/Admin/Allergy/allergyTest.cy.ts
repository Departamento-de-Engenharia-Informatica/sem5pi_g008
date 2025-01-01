import json from '../../../globalVariables.json';

describe('Allergy Tests', () => {

  beforeEach(() => {
    cy.clearCookies();
    cy.setCookie('.AspNetCore.Cookies', json.Cookies.Admin);
    cy.visit('/admin/allergyManagement');
  });

  it('Visits the Allergy Management Component', () => {
    cy.contains('h1', 'Allergy Management');
  });

  it('Update Designation of an Allergy', () => {
    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialDesignation = allergy.find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Designation:'))
        .text()
        .replace('Designation:', '')
        .trim();
      const updatedDesignation = `Updated ${initialDesignation}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#designation').clear().type(updatedDesignation);

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list .allergy-item').first().find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Designation:'))
        .should('have.text', `Designation: Updated ${initialDesignation}`);
    });
  });

  it('Update Description of an Allergy', () => {
    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialDescription = allergy.find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Description:'))
        .text()
        .replace('Description:', '')
        .trim();
      const updatedDescription = `Updated ${initialDescription}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#description').clear().type(updatedDescription);

      cy.get('[type="submit"]').click();

      cy.get('.allergy-list .allergy-item').first().find('.allergy-details p')
        .filter((index, p) => p.innerText.includes('Description:'))
        .should('have.text', `Description: Updated ${initialDescription}`);
    });
  });

  it('Add and Remove an Effect from an Allergy', () => {
    const minuteAndSecond = new Date().toISOString().slice(14, 19).replace(':', '');

    cy.get('.allergy-list .allergy-item').first().then((allergy) => {
      const initialEffects = allergy.find('.allergy-details ul li').map((index, el) => el.innerText).get();

      const newEffect = `Effect ${minuteAndSecond}`;

      cy.wrap(allergy).find('.edit-button').click();

      cy.get('#effect').clear().type(newEffect);
      cy.get('.effect-input button').click();
      cy.get('.effects-list').should('contain', newEffect);
      cy.get('.effects-list .effect-item').first().find('button').click();
      cy.get('.effects-list').should('not.equal', initialEffects[0]);
      cy.get('[type="submit"]').click();
      cy.get('.allergy-list .allergy-item').first().find('.allergy-details ul li')
        .should('contain', newEffect)
        .and('not.contain', initialEffects[0]);
    });
  });




});
