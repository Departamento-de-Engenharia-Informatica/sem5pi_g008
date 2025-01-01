import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { AllergyService } from './allergyService';
import { HttpErrorResponse } from '@angular/common/http';
import json from '../../appsettings.json';

describe('AllergyService', () => {
  let service: AllergyService;
  let httpMock: HttpTestingController;
  const apiUrl = json.backendApi['2'].url;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [AllergyService]
    });
    service = TestBed.inject(AllergyService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    // Verify that no requests are outstanding
    httpMock.verify();
  });

  it('should update allergy designation', () => {
    const allergyId = '123';
    const newDesignation = 'New Designation';

    service.updateAllergyDesignation(allergyId, newDesignation).subscribe((response: any) => {
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${apiUrl}/allergy/${allergyId}/designation`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ designation: newDesignation });
  });

  it('should update allergy description', () => {
    const allergyId = '123';
    const newDescription = 'New Description';

    service.updateAllergyDescription(allergyId, newDescription).subscribe((response: any) => {
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${apiUrl}/allergy/${allergyId}/description`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ description: newDescription });
  });

  it('should update allergy effects', () => {
    const allergyId = '123';
    const newEffects = ['Effect1', 'Effect2'];

    service.updateAllergyEffects(allergyId, newEffects).subscribe((response: any) => {
      expect(response).toBeUndefined(); // Expect void return type
    });

    const req = httpMock.expectOne(`${apiUrl}/allergy/${allergyId}/effects`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ effects: newEffects });
  });
});
