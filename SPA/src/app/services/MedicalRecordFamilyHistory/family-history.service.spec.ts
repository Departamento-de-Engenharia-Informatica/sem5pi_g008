import { TestBed } from '@angular/core/testing';

import { FamilyHistoryService } from './family-history.service';

describe('FamilyHistoryService', () => {
  let service: FamilyHistoryService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(FamilyHistoryService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
