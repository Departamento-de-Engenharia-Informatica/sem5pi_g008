import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { AllergyService } from './allergyService';
import { HttpErrorResponse } from '@angular/common/http';
import json from '../../appsettings.json';
import {CreateAllergyDTO} from '../../DTOs/createDTOs/createAllergyDTO';
import {AllergyMapper} from '../../DTOs/mappers/allergyMapper';

describe('AllergyService', () => {
  let service: AllergyService;
  let httpMock: HttpTestingController;
  const apiUrl = json.backendApi['2'].url;
  let mockMapper: jasmine.SpyObj<typeof AllergyMapper>;

  beforeEach(() => {
    mockMapper = jasmine.createSpyObj('AllergyMapper', ['createDtoToDomain', 'domainToBackendDto']);

    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [AllergyService]
    });
    service = TestBed.inject(AllergyService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
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
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${apiUrl}/allergy/${allergyId}/effects`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ effects: newEffects });
  });

  it('should create allergy', () => {

    const mockCreateAllergyDTO: CreateAllergyDTO = {
      allergyCode: 'A12',
      allergyDesignation: 'Allergy-Designation',
      allergyDescription: 'Allergy-Description',
      allergyEffects: ['Effect1', 'Effect2']
    };

    const mockAllergy = {
      domainId: -1,
      code: 'A12',
      designation: 'Allergy-Designation',
      description: 'Allergy-Description',
      effects: ['Effect1', 'Effect2']
    };

    const mockResponse = 'Allergy created successfully';

    mockMapper.createDtoToDomain.and.returnValue({ ...mockAllergy });
    mockMapper.domainToBackendDto.and.returnValue({ ...mockAllergy });

    service.addAllergy(mockCreateAllergyDTO).subscribe((response: any) => {
      expect(response).toBe(mockResponse);
    });

    const req = httpMock.expectOne(`${apiUrl}/allergy`);
    expect(req.request.method).toBe('POST');
    expect(req.request.body.code).toEqual(mockAllergy.code);
    expect(req.request.body.designation).toEqual(mockAllergy.designation);
    expect(req.request.body.description).toEqual(mockAllergy.description);
    expect(req.request.body.effects).toEqual(mockAllergy.effects);
    expect(req.request.withCredentials).toBeTrue();
  });
});
