import { TestBed } from '@angular/core/testing';

import { MedicalRecordMedicalConditionService } from './medical-record-medical-condition.service';

describe('MedicalRecordMedicalConditionService', () => {
  let service: MedicalRecordMedicalConditionService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(MedicalRecordMedicalConditionService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
